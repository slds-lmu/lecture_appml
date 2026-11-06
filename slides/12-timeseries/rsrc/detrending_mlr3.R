# ---------------------------------------------------------------------------
# Detrending inside an mlr3 pipeline (leakage-safe)
#
# Demonstrates how to use PipeOpTargetTrafo to fit a linear trend on each
# training fold only, residualize the target, train a tree model on residuals,
# and invert predictions back to the original scale.
#
# Reference: https://chatgpt.com/share/69dc97c5-ea14-838f-9a42-5aa4c7cba008
# ---------------------------------------------------------------------------

library(data.table)
library(R6)
library(mlr3)
library(mlr3pipelines)
library(mlr3learners)
library(paradox)
if (!requireNamespace("logging", quietly = TRUE)) {
  # Fallback: define minimal logging functions
  loginfo <- function(fmt, ...) message(sprintf(fmt, ...))
} else {
  library(logging)
  basicConfig()
}

set.seed(509)
root <- rprojroot::find_root(rprojroot::is_git_root)

# ---------------------------------------------------------------------------
# 1. Load and prepare bike sharing data (daily aggregation)
# ---------------------------------------------------------------------------
bike <- fread(file.path(root, "data", "bike_sharing_demand.csv"))
bike[, datetime := as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")]
bike <- bike[!is.na(datetime)]
bike[, date := as.IDate(datetime)]

bike_daily <- bike[, .(daily_count = sum(count)), by = date]
bike_daily[, t := as.numeric(date)]

loginfo("Loaded %d daily observations", nrow(bike_daily))

# ---------------------------------------------------------------------------
# 2. Custom PipeOpTargetTrafo for linear detrending
# ---------------------------------------------------------------------------
PipeOpTargetDetrendLM <- R6Class(
  "PipeOpTargetDetrendLM",
  inherit = PipeOpTargetTrafo,

  public = list(
    initialize = function(id = "targetdetrend", param_vals = list()) {
      ps <- paradox::ps(
        time_col = paradox::p_uty(tags = "train")
      )
      super$initialize(
        id = id,
        param_set = ps,
        param_vals = param_vals
      )
    }
  ),

  private = list(
    .get_state = function(task) {
      col <- self$param_set$values$time_col
      if (is.null(col)) stop("Set `time_col` parameter.")

      y <- task$truth()
      t_vals <- task$data(cols = col)[[1L]]

      fit <- stats::lm(y ~ t_vals)
      loginfo("Detrend: fit on %d training rows, intercept=%.2f, slope=%.4f",
              length(y), coef(fit)[1], coef(fit)[2])

      list(fit = fit, time_col = col)
    },

    .transform = function(task, phase) {
      col <- self$state$time_col
      t_vals <- task$data(cols = col)[[1L]]
      trend <- stats::predict(self$state$fit, newdata = data.frame(t_vals = t_vals))

      resid <- task$truth() - trend

      # Replace target with residuals
      dt <- task$data()
      target_name <- task$target_names
      dt[[target_name]] <- resid

      TaskRegr$new(
        id = task$id,
        backend = dt,
        target = target_name
      )
    },

    .train_invert = function(task) {
      col <- self$state$time_col
      t_vals <- task$data(cols = col)[[1L]]
      trend <- stats::predict(self$state$fit, newdata = data.frame(t_vals = t_vals))

      list(truth = task$truth(), trend = trend)
    },

    .invert = function(prediction, predict_phase_state) {
      mlr3::PredictionRegr$new(
        row_ids = prediction$row_ids,
        truth = predict_phase_state$truth,
        response = prediction$response + predict_phase_state$trend
      )
    }
  )
)

# ---------------------------------------------------------------------------
# 3. Build the pipeline: detrend -> learner -> invert
# ---------------------------------------------------------------------------
task <- TaskRegr$new(
  id = "bike_daily",
  backend = bike_daily[, .(t, daily_count)],
  target = "daily_count"
)

loginfo("Task: %d rows, target='%s'", task$nrow, task$target_names)

# Base learner
base_learner <- lrn("regr.ranger", num.trees = 500)

# Detrending pipeline
detrend_op <- PipeOpTargetDetrendLM$new(
  param_vals = list(time_col = "t")
)

graph <- ppl(
  "targettrafo",
  graph = base_learner,
  trafo_pipeop = detrend_op
)

graph_learner <- as_learner(graph)
graph_learner$id <- "detrended_ranger"

# Plain learner for comparison
plain_learner <- lrn("regr.ranger", num.trees = 500)
plain_learner$id <- "plain_ranger"

# ---------------------------------------------------------------------------
# 4. Evaluate with time-series-aware CV (ordered holdout splits)
# ---------------------------------------------------------------------------
# mlr3's built-in CV is iid-shuffled, so we build ordered splits manually
n <- task$nrow
test_size <- 28  # 4 weeks
n_splits <- 5

custom_rsmp <- rsmp("custom")

train_sets <- list()
test_sets <- list()
for (i in seq_len(n_splits)) {
  test_end <- n - (n_splits - i) * test_size
  test_start <- test_end - test_size + 1
  train_end <- test_start - 1
  train_sets[[i]] <- seq_len(train_end)
  test_sets[[i]] <- test_start:test_end
}

custom_rsmp$instantiate(task, train_sets, test_sets)

loginfo("Resampling: %d folds, test_size=%d", n_splits, test_size)

# ---------------------------------------------------------------------------
# 5. Benchmark: detrended vs plain
# ---------------------------------------------------------------------------
design <- benchmark_grid(
  tasks = task,
  learners = list(graph_learner, plain_learner),
  resamplings = custom_rsmp
)

bmr <- benchmark(design)

results <- bmr$aggregate(msr("regr.rmse"))
loginfo("Benchmark results:")
for (i in seq_len(nrow(results))) {
  loginfo("  %s: RMSE = %.2f", results$learner_id[i], results$regr.rmse[i])
}

cat("\n=== Benchmark Results ===\n")
print(results[, .(learner_id, regr.rmse)])
