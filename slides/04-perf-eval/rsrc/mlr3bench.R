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
aggr = aggr %>%
  mutate(learner_id = replace(learner_id, learner_id == "encode.classif.featureless", "featureless")) %>%
  mutate(learner_id = replace(learner_id, learner_id == "encode.classif.ranger", "ranger")) %>%
  mutate(learner_id = replace(learner_id, learner_id == "encode.classif.kknn", "kknn")) %>%
  mutate(learner_id = replace(learner_id, learner_id == "encode.classif.rpart", "rpart")) %>%
  mutate(learner_id = replace(learner_id, learner_id == "encode.classif.svm", "svm")) %>%
  mutate(learner_id = replace(learner_id, learner_id == "encode.classif.cv_glmnet", "cv_glmnet"))
# autoplot(bmr)

library(tidyverse)
ranktable = aggr %>%
  group_by(task_id) %>%
  mutate(rank_on_task = rank(classif.ce)) %>%
  mutate(ce_rank = paste(round(classif.ce, 4), " (", rank_on_task, ")", sep =  ""))


averageranks = ranktable %>%
  group_by(learner_id) %>%
  summarize(average_rank_on_task = mean(rank_on_task))

averageranks

meanrank = (1 / nrow(ranktable)) * sum(ranktable$rank_on_task)
meanrank

sstotal = length(tasklist) *
  sum((averageranks$average_rank_on_task- meanrank)^2)
sstotal

sserror = (1 / (length(tasklist) * (length(learners) - 1))) * sum((ranktable$rank_on_task - meanrank)^2)
sserror

friedmanstat = sstotal / sserror
friedmanstat
qchisq( .99, df = 5)        # 7 degrees of freedom

library(mlr3benchmark)
obj1 = as_benchmark_aggr(bmr, measures = msr("classif.ce"))
obj1$friedman_posthoc()

aggr_wide = ranktable %>%
  select(c(task_id, learner_id, ce_rank)) %>%
  pivot_wider(names_from = learner_id, values_from = ce_rank)
aggr_wide

library(xtable)
print(
  xtable(
    aggr_wide,
    type = "latex",
    digits = 4
  ),
  file = "slides/04-perf-eval/rsrc/friedman_benchmark_results.tex")

print(
  xtable(
    aggr_wide[1:2, ],
    type = "latex",
    digits = 4
  ),
  file = "slides/04-perf-eval/rsrc/friedman_benchmark_results_short.tex")


ranktable_short_wide = aggr %>%
  filter(learner_id == "rpart" | learner_id == "ranger") %>%
  group_by(task_id) %>%
  mutate(rank_on_task = rank(classif.ce)) %>%
  mutate(ce_rank = paste(round(classif.ce, 4), " (", rank_on_task, ")", sep =  "")) %>%
  select(c(task_id, learner_id, ce_rank)) %>%
  pivot_wider(names_from = learner_id, values_from = ce_rank)
ranktable_short_wide = ranktable_short_wide[1:6, ]

library(xtable)
print(
  xtable(
    ranktable_short_wide,
    type = "latex",
    digits = 4
  ),
  file = "slides/04-perf-eval/rsrc/friedman_benchmark_results_short.tex")

library(ggplot2)
aggr.ggplot = aggr %>%
  group_by(task_id) %>%
  arrange(classif.ce, .by_group = TRUE) %>%
  mutate(position = rank(classif.ce, ties.method= "first"))

cbb_palette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7")

task_order = lapply(tasklist, FUN = function(x) x$data_name)

benchplot = ggplot(
  aggr.ggplot,
  aes(x = factor(task_id, level = task_order), y = classif.ce, group = position)) +
  geom_col(
    color = "black",
    linewidth = 0.2,
    aes(fill = learner_id),
    position = position_dodge(),
    width = 0.75) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_manual(values = cbb_palette) +
  xlab("task_id")
benchplot

ggsave(
  filename = "slides/04-perf-eval/figure/benchmarkcolplot.png",
  plot = benchplot,
  width = 9, height = 6)

averageranks = averageranks %>%
  arrange(average_rank_on_task)

averagerankplot = ggplot(averageranks, aes(x = factor(learner_id, level = learner_id), y = average_rank_on_task)) +
  geom_col(aes(fill = learner_id),
           position = position_dodge()) +
  xlab("learner_id") +
  ylab("average rank") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_manual(values = cbb_palette)

averagerankplot

ggsave(
  filename = "slides/04-perf-eval/figure/benchmarkrankplot.png",
  plot = averagerankplot,
  width = 6, height = 4)

