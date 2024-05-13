library(mlr3oml)
library(mlr3verse)
library(mlr3learners)
library(tidyverse)
library(ggplot2)

x1 = seq(-3, 3, 0.1)
d1 = dnorm(x1, 0, 1)
x2 = seq(-1, 5, 0.1)
d2 = dnorm(x2, 2, 1)
rv_standard_normal = data.frame(x = x1, d = d1, var = "x1")
rv_standard_normal = rbind(rv_standard_normal, data.frame(x = x2, d = d2, var = "x2"))
cbb_palette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7")

normal_density_plot = ggplot(data = rv_standard_normal, aes(x = x, y = d, color = var, linetype = var)) +
  geom_line(linewidth = 1) +
  theme_bw() +
  scale_color_manual("", values = c(x1 = cbb_palette[2], x2 = cbb_palette[3])) +
  scale_linetype_manual("", values = c("solid", "longdash")) +
  theme(legend.position = "none") +
  geom_segment(
    color = cbb_palette[2],
    x = 0, xend = 0, y = 0, yend = max(rv_standard_normal$d)) +
  geom_segment(
    color = cbb_palette[3],
    x = 2, xend = 2, y = 0, yend = max(rv_standard_normal$d)) +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20))
normal_density_plot
# ggsave(
#   normal_density_plot,
#   file = "slides/04-perf-eval/figure/normal_densities.png",
#   width = 7,
#   height = 5)


#
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

resamplings = rsmp("cv", folds = 3)
design = benchmark_grid(tasklist, learners, resamplings)

# set.seed(123)
# bmr = benchmark(design)
# save(bmr, file = "slides/04-perf-eval/rsrc/friedman_example_benchmark.Rdata")
load("slides/04-perf-eval/rsrc/friedman_example_benchmark.Rdata")

aggr = bmr$aggregate()
aggr = aggr %>%
  mutate(learner_id = replace(learner_id, learner_id == "encode.classif.featureless", "featureless")) %>%
  mutate(learner_id = replace(learner_id, learner_id == "encode.classif.ranger", "ranger")) %>%
  mutate(learner_id = replace(learner_id, learner_id == "encode.classif.kknn", "kknn")) %>%
  mutate(learner_id = replace(learner_id, learner_id == "encode.classif.rpart", "rpart")) %>%
  mutate(learner_id = replace(learner_id, learner_id == "encode.classif.svm", "svm")) %>%
  mutate(learner_id = replace(learner_id, learner_id == "encode.classif.cv_glmnet", "cv_glmnet"))

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
# sanity check with mlr3
library(mlr3benchmark)
benchmark_aggr = as_benchmark_aggr(bmr, measures = msr("classif.ce"))
benchmark_aggr$friedman_test()
cd_plot = autoplot(benchmark_aggr, type = "cd", meas = "ce", minimize = TRUE)

# ggsave(
  # cd_plot,
  # file = "slides/04-perf-eval/figure/crit_diff_plot.png",
  # height = 2,
  # width = 10,
  # units = "in",
  # dpi = 300)

# critical value for Friedman omnibus test

# post hoc tests
library(PMCMRplus)

ce_table_wide = ranktable %>%
  select(c(task_id, learner_id, classif.ce)) %>%
  pivot_wider(names_from = learner_id, values_from = classif.ce) %>%
  ungroup() %>%
  select(-task_id) %>%
  as.matrix()
ce_table_wide
# sanity check with stat package
friedman.test(ce_table_wide)
# posthoc nemenyi test
# ?frdAllPairsNemenyiTest
rownames(ce_table_wide) = LETTERS[1:nrow(ce_table_wide)]
nemenyi_test = frdAllPairsNemenyiTest(y = ce_table_wide)
nemenyi_test$statistic
nemenyi_test$p.value

mean_diff_rpart_ranger = averageranks[averageranks$learner_id == "rpart", 2] - averageranks[averageranks$learner_id == "ranger", 2]
# mean_diff_rpart_ranger / sqrt(16 * 6 * 7 / 12)
#
# nemenyi_stat = (abs(mean_diff_rpart_ranger$average_rank_on_task) / sqrt((6 * 7) / 6 * 16)) * sqrt(2)
# nemenyi_stat
#
# 1 - ptukey(nemenyi_stat, nmeans = 6, df = Inf)
# # critical mean rank difference
# NSM3::cRangeNor(alpha = 0.05, k = 6)
# qtukey(p = 0.05, df = Inf, nmeans = 6, lower.tail = FALSE / sqrt(2))
crit_value_mean_rank_diff = (qtukey(p = 0.05, df = Inf, nmeans = 6, lower.tail = FALSE) / sqrt(2)) * (sqrt((6 * (6 + 1)) / (6 * 16)))
crit_value_mean_rank_diff
# reject H_0!

# bonferroni dunn test
dunn_stat = (abs(mean_diff_rpart_ranger$average_rank_on_task) / sqrt((6 * 7) / 6 * 16))
# prob of observing dunn statistic under h0:
pnorm(dunn_stat, 0, 1, lower.tail = FALSE)
# do not reject H_0

aggr_wide = ranktable %>%
  select(c(task_id, learner_id, ce_rank)) %>%
  pivot_wider(names_from = learner_id, values_from = ce_rank)
aggr_wide

library(xtable)
# print(
#   xtable(
#     aggr_wide,
#     type = "latex",
#     digits = 4
#   ),
#   file = "slides/04-perf-eval/rsrc/friedman_benchmark_results.tex")

# print(
#   xtable(
#     aggr_wide[1:2, ],
#     type = "latex",
#     digits = 4
#   ),
#   file = "slides/04-perf-eval/rsrc/friedman_benchmark_results_short.tex")


ranktable_wide_rpart_ranger = aggr %>%
  filter(learner_id == "rpart" | learner_id == "ranger") %>%
  group_by(task_id) %>%
  mutate(rank_on_task = rank(classif.ce)) %>%
  mutate(ce_rank = paste(round(classif.ce, 4), " (", rank_on_task, ")", sep =  "")) %>%
  select(c(task_id, learner_id, ce_rank)) %>%
  pivot_wider(names_from = learner_id, values_from = ce_rank)
ranktable_wide_rpart_ranger = ranktable_wide_rpart_ranger[1:6, ]
ranktable_wide_rpart_ranger

library(xtable)
# print(
#   xtable(
#     ranktable_wide_rpart_ranger,
#     type = "latex",
#     digits = 4
#   ),
#   file = "slides/04-perf-eval/rsrc/friedman_benchmark_results_rpart_ranger.tex")

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

# ggsave(
#   filename = "slides/04-perf-eval/figure/benchmarkcolplot.png",
#   plot = benchplot,
#   width = 9, height = 6)

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

# ggsave(
#   filename = "slides/04-perf-eval/figure/benchmarkrankplot.png",
#   plot = averagerankplot,
#   width = 6, height = 4)

# t test of rpart and ranger on strikes task

rr = aggr$resample_result
kin8nm.task = rr[which(aggr$task_id == "kin8nm")][[3]]$task
# rr.rpart = rr[which(aggr$task_id == "strikes")][[3]]
# rr.ranger = rr[which(aggr$task_id == "strikes")][[4]]

rpart.model = mlr_learners$get("classif.rpart")
rpart.model$predict_type = "prob"
rpart.model$train(kin8nm.task)
rpart.predictions = rpart.model$predict_newdata(newdata = kin8nm.task$data())$print()

ranger.model = mlr_learners$get("classif.ranger")
ranger.model$predict_type = "prob"
ranger.model$train(kin8nm.task)
ranger.predictions = ranger.model$predict_newdata(newdata = kin8nm.task$data())$print()

conf.mat = rpart.predictions[ , c("row_ids", "truth", "response", "prob.P")]
colnames(conf.mat) = c("id", "truth", "rpart", "rpart_prob")
conf.mat$ranger = ranger.predictions$response
conf.mat$ranger_prob = ranger.predictions$prob.P
conf.mat

conf.mat = conf.mat %>%
  rowwise() %>%
  # mutate(
  #   rpart_prob = max(.Machine$double.eps, min(1 - .Machine$double.eps, rpart_prob))
  # ) %>%
  # mutate(
  #   ranger_prob = max(.Machine$double.eps, min(1 - .Machine$double.eps, ranger_prob))
  # ) %>%
  mutate(
    rpart_correct = case_when(
      truth == "P" & rpart == "P" |
      truth == "N" & rpart == "N"
      ~ 1,
      .default = 0)) %>%
  mutate(
    ranger_correct = case_when(
      truth == "P" & ranger == "P" |
        truth == "N" & ranger == "N"
      ~ 1,
      .default = 0)) %>%
  mutate(
    both_correct = case_when(
      rpart_correct == 1 & ranger_correct == 1
      ~ 1,
      .default = 0)) %>%
  mutate(
    both_wrong = case_when(
      rpart_correct == 0 & ranger_correct == 0
      ~ 1,
      .default = 0)) %>%
  mutate(
    only_rpart_correct = case_when(
      rpart_correct == 1 & ranger_correct == 0
      ~ 1,
      .default = 0)) %>%
  mutate(
    only_ranger_correct = case_when(
      rpart_correct == 0 & ranger_correct == 1
      ~ 1,
      .default = 0)) %>%
  mutate(
    rpart_loss = (rpart_prob - ifelse(truth == "P", 1, 0))^2
    ) %>%
  mutate(
    ranger_loss = (ranger_prob - ifelse(truth == "P", 1, 0))^2
    ) %>%
  mutate(
    diff_loss = rpart_loss - ranger_loss
  )

conf.mat

conf.mat %>%
  select(c(rpart_loss, ranger_loss)) %>%


ggplot(conf.mat %>% select(c(rpart_loss, ranger_loss))) +
  geom_density()

conf.mat.subset = conf.mat %>%
  ungroup() %>%
  slice(c(1, 2, nrow(conf.mat))) %>%
  select(id, truth, rpart_prob, ranger_prob, rpart_loss, ranger_loss, diff_loss)
conf.mat.subset

# library(xtable)
# print(
#   xtable(
#     conf.mat.subset,
#     type = "latex",
#     digits = 4
#   ),
#   include.rownames = FALSE,
#   file = "slides/04-perf-eval/rsrc/rpart_ranger_kin8nm_task.tex")

# t-test

mean_diff_loss = conf.mat %>%
  ungroup() %>%
  summarize(diff_loss = mean(diff_loss))
mean_diff_loss

t_var = sqrt((1 / (nrow(conf.mat) - 1)) * sum((conf.mat$diff_loss - as.numeric(mean_diff_loss))^2))
t_var

t_statistic = sqrt(nrow(conf.mat)) * (mean_diff_loss / t_var)
t_statistic
# lower critical value, reject H0 if t-stat smaller
qt(0.025, df = nrow(conf.mat) - 1)
# OR
# upper critical value, reject H0 if t-stat larger
qt(0.025, df = nrow(conf.mat) - 1)

# do not reject H0 !

# McNemar

conf.summary = conf.mat %>%
  ungroup() %>%
  summarize(
    both_correct = sum(both_correct),
    both_wrong = sum(both_wrong),
    only_rpart_correct = sum(only_rpart_correct),
    only_ranger_correct = sum(only_ranger_correct),
)

conf.summary

mcnemar_stat = (abs(conf.summary$only_rpart_correct - conf.summary$only_ranger_correct) - 1)^2 / (conf.summary$only_rpart_correct + conf.summary$only_ranger_correct)
mcnemar_stat

# critical value for alpha = 5%, reject H0 if mcnemar_stat larger
qchisq(0.95, df = 1)

# reject H0!

