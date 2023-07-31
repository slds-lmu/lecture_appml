library(mlr3)
library(mlr3learners)
library(mlr3pipelines)

# FIXME: when does classifavg do soft or hard averaging?

set.seed(1)
task = tsk("sonar")
task$col_roles$stratum = task$target_names
resampling = rsmp("holdout")$instantiate(task)
measure = msr("classif.bacc")

task_train = task$clone(deep = TRUE)$filter(resampling$train_set(1))
task_test = task$clone(deep = TRUE)$filter(resampling$test_set(1))

ranger = lrn("classif.ranger")
ranger$predict_type = "prob"
kknn = lrn("classif.kknn")
kknn$predict_type = "prob"
log_reg = lrn("classif.log_reg")
log_reg$predict_type = "prob"
featureless = lrn("classif.featureless")
featureless$predict_type = "prob"

ensemble = as_learner(gunion(list(ranger, kknn, log_reg)) %>>% po("classifavg"))
ensemble$param_set$values$classifavg.weights = rep(1/3, 3)

ensemble_opt = as_learner(gunion(list(ranger, kknn, log_reg)) %>>% po("classifavg"))
ensemble_opt$id = paste0(ensemble_opt$id, ".optimized")
ensemble_opt$param_set$values$classifavg.weights = c(0.85, 0.10, 0.05)

benchmark_grid = benchmark_grid(task, learner = list(ranger, kknn, log_reg, featureless, ensemble, ensemble_opt), resampling = resampling)
benchmark_result = benchmark(benchmark_grid)
benchmark_result$score(measure)

library(bbotk)

train_task = task$clone(deep = TRUE)$filter(resampling$train_set(1))
test_task = task$clone(deep = TRUE)$filter(resampling$test_set(1))

ranger$train(train_task)
ranger_predict = ranger$predict(test_task)
kknn$train(train_task)
kknn_predict= kknn$predict(test_task)
log_reg$train(train_task)
log_reg_predict = log_reg$predict(test_task)

normalize_weights = function(weights) {
  if (inherits(weights, what = "list")) {
    weights = unlist(weights)
  }
  weights = weights / sum(weights)
}

fun = function(xs) {
  weights = normalize_weights(xs)
  reduce_probs = function(p, w) {
    p$prob[, 1] * w
  }
  prob = Reduce("+", x = Map(reduce_probs, list(ranger_predict, kknn_predict, log_reg_predict), weights))
  prob = cbind(prob, 1 - prob)
  colnames(prob) = test_task$class_names
  ensemble_prediction = PredictionClassif$new(task = test_task, response = NULL, prob = prob)
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

optimizer = opt("random_search")
optimizer$optimize(instance)

weights = normalize_weights(instance$archive$best()$x_domain[[1L]])
