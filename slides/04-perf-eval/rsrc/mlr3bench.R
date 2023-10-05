library(mlr3oml)
library(mlr3verse)
library(mlr3learners)
library(tidyverse)

otasks = list_oml_tasks(
  type = "classif",
  number_features = c(5, 10),
  number_instances = c(500, 10000),
  number_classes = 2,
  number_missing_values = 0
)

# < 1.2 filtern

otasks$name
otasks = otasks %>%
  filter(data_id != 720) %>%
  filter(MajorityClassSize / MinorityClassSize < 1.2) %>%
  group_by(name) %>%
  filter(row_number(data_id) == 1) %>%
  group_by(data_id) %>%
  filter(row_number(data_id) == 1)
otasks
nrow(otasks)

set.seed(123)
otasks = otasks[sample(nrow(otasks), 16), ]
tasklist = lapply(otasks$task_id, FUN = function(x) otsk(x))
length(tasklist)
unlist(lapply(tasklist, FUN = function(x) x[["data_name"]]))

tasklist

learners = lapply(c("classif.featureless",  "classif.cv_glmnet", "classif.rpart", "classif.ranger", "classif.kknn", "classif.svm"), lrn)
poe = po("encode", method = "one-hot")
learners = lapply(learners, FUN = function(x) {po("encode") %>>% x})
learners

resamplings = rsmp("cv", folds = 3)
design = benchmark_grid(tasklist, learners, resamplings)
print(design)

set.seed(123)
bmr = benchmark(design)
# save(bmr, file = "slides/04-perf-eval/rsrc/friedman_example_benchmark.Rdata")
aggr = bmr$aggregate()
print(aggr)

# autoplot(bmr)
aggr
aggr_wide = aggr %>%
  select(c(task_id, learner_id, classif.ce)) %>%
  pivot_wider(names_from = learner_id, values_from = classif.ce)
aggr_wide
colnames(aggr_wide) = c("featureless",  "cv_glmnet", "rpart", "ranger", "kknn", "svm")
library(xtable)
print(xtable(aggr_wide, type = "latex"), file = "slides/04-perf-eval/rsrc/friedman_benchmark_results.tex")

class(aggr)
library(mlr3benchmark)
obj1 = as_benchmark_aggr(bmr, measures = msr("classif.ce"))

library(tidyverse)
ranktable = aggr %>%
  group_by(task_id) %>%
  mutate(rank_on_task = dense_rank(classif.ce))

ranktable

averageranks = ranktable %>%
  group_by(learner_id) %>%
  summarize(average_rank_on_task = mean(rank_on_task))

averageranks

meanrank = (1 / nrow(ranktable)) * sum(ranktable$rank_on_task)

sstotal = length(tasklist) *
  sum((averageranks$average_rank_on_task- meanrank)^2)
sstotal

sserror = (1 / (length(tasklist) * (length(learners) - 1))) * sum((ranktable$rank_on_task - meanrank)^2)
sserror

friedmanstat = sstotal / sserror
friedmanstat

obj1$friedman_posthoc()
