library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(bbotk)
library(mlr3misc)
library(data.table)

lgr::get_logger("bbotk")$set_threshold("warn")

# FIXME: when does classifavg do soft or hard averaging?
set.seed(1234)
task = tsk("sonar")
task$col_roles$stratum = task$target_names
resampling_outer = rsmp("holdout")$instantiate(task)
measure = msr("classif.bacc")

inner_task = task$clone(deep = TRUE)$filter(resampling_outer$train_set(1))
resampling_inner = rsmp("holdout")$instantiate(inner_task)

task_train = inner_task$clone(deep = TRUE)$filter(resampling_inner$train_set(1))
task_valid = inner_task$clone(deep = TRUE)$filter(resampling_inner$test_set(1))

ranger = lrn("classif.ranger", seed = 42)  # need this
ranger$predict_type = "prob"
kknn = lrn("classif.kknn")
kknn$predict_type = "prob"
log_reg = lrn("classif.log_reg")
log_reg$predict_type = "prob"
featureless = lrn("classif.featureless")
featureless$predict_type = "prob"

ensemble = as_learner(gunion(list(ranger, kknn, log_reg)) %>>% po("classifavg"))
ensemble$param_set$values$classifavg.weights = rep(1 / 3, 3)

ensemble_opt_fixed = as_learner(gunion(list(ranger, kknn, log_reg)) %>>% po("classifavg"))
ensemble_opt_fixed$id = paste0(ensemble_opt_fixed$id, ".optimized_fixed")
ensemble_opt_fixed$param_set$values$classifavg.weights = c(0.75, 0.05, 0.20)

ensemble_opt_ges = as_learner(gunion(list(ranger, kknn, log_reg)) %>>% po("classifavg"))
ensemble_opt_ges$id = paste0(ensemble_opt_ges$id, ".optimized_ges")
ensemble_opt_ges$param_set$values$classifavg.weights = c(0.66, 0.17, 0.17)  # ges

benchmark_grid_test = benchmark_grid(task, learner = list(ranger, kknn, log_reg, featureless, ensemble, ensemble_opt_fixed, ensemble_opt_ges), resampling = resampling_outer)
benchmark_result_test = benchmark(benchmark_grid_test)

measures_test = benchmark_result_test$score(measure)
measures_test[, classif.bacc := round(classif.bacc, 3)]
print(measures_test)

benchmark_grid_valid = benchmark_grid(inner_task, learner = list(ranger, kknn, log_reg, featureless, ensemble, ensemble_opt_fixed, ensemble_opt_ges), resampling = resampling_inner)
benchmark_result_valid = benchmark(benchmark_grid_valid)

measures_valid = benchmark_result_valid$score(measure)
measures_valid[, classif.bacc := round(classif.bacc, 3)]
print(measures_valid)

ranger$train(task_train)
ranger_predict = ranger$predict(task_valid)
kknn$train(task_train)
kknn_predict = kknn$predict(task_valid)
log_reg$train(task_train)
log_reg_predict = log_reg$predict(task_valid)

normalize_weights = function(weights) {
  if (inherits(weights, what = "list")) {
    weights = unlist(weights)
  }
  weights[is.na(weights) | weights == 0] = 1
  weights = weights / sum(weights)
  weights
}

fun = function(xs) {
  weights = normalize_weights(xs)
  reduce_probs = function(p, w) {
    p$prob[, 1] * w
  }
  prob = pmax(pmin(Reduce("+", x = Map(reduce_probs, list(ranger_predict, kknn_predict, log_reg_predict), weights)), 1), 0)
  prob = cbind(prob, 1 - prob)
  colnames(prob) = task_valid$class_names
  ensemble_prediction = PredictionClassif$new(task = task_valid, response = NULL, prob = prob)
  list(classif.bacc = unname(ensemble_prediction$score(measure)))
}

domain = ps(w1 = p_dbl(0, 1), w2 = p_dbl(0, 1), w3 = p_dbl(0, 1))
codomain = ps(classif.bacc = p_dbl(0, 1, tags = "maximize"))

objective = ObjectiveRFun$new(
  fun = fun,
  domain = domain,
  codomain = codomain,
  id = "ensemble_weigths"
)

instance = OptimInstanceSingleCrit$new(
  objective = objective,
  terminator = trm("evals", n_evals = 1000)
)

optimizer = opt("cmaes")
optimizer$optimize(instance)

cmaes_weights = normalize_weights(instance$archive$best()$x_domain[[1L]])
print(cmaes_weights)

# GES
get_ges_weights = function(selected, learner_ids) {
  selected = selected[selected != 0]
  n = length(selected)
  weights = map_dbl(learner_ids, function(learner_id) {
    sum(learner_id == selected) / n
  })
  weights
}

ges = function(learner_predicts, task_valid, iterations = 100L) {
  iteration = 0
  learner_ids = seq_along(learner_predicts)
  performance = numeric(iterations)
  selected = integer(iterations)
  for (iteration in seq_len(iterations)) {
    cat("Iteration: ", iteration, "\n")
    performance_tmp = map_dbl(learner_ids, function(learner_id) {
      selected_tmp = selected
      selected_tmp[iteration] = learner_id
      weights = get_ges_weights(selected_tmp, learner_ids = learner_ids)
      reduce_probs = function(p, w) {
        p$prob[, 1] * w
      }
      prob = Reduce("+", x = Map(reduce_probs, learner_predicts, weights))
      prob = cbind(prob, 1 - prob)
      colnames(prob) = task_valid$class_names
      ensemble_prediction = PredictionClassif$new(task = task_valid, response = NULL, prob = prob)
      ensemble_prediction$score(measure)
    })
    cat("Performance if learner added: ", round(performance_tmp, 3), "\n")
    select = which.max(performance_tmp)
    performance[iteration] = performance_tmp[select]
    selected[iteration] = select
  }
  best = which.max(performance)
  weights = get_ges_weights(selected[1:best], learner_ids = learner_ids)
  list(performance_best = performance[best], weights = weights, selected = selected, performance = performance)
}

ges_results = ges(list(ranger_predict, kknn_predict, log_reg_predict), task_valid = task_valid, iterations = 10L)
print(ges_results$weights)
