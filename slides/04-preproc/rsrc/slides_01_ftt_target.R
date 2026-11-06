library(mlr)
library(mlrCPO)
library(ggplot2)
library(dplyr)
theme_set(theme_minimal())
root = rprojroot::find_root(rprojroot::is_git_root)

data = readr::read_csv(paste0(root, "/data/ames_housing_extended.csv"))
colnames(data)[1] = "X1"
data = data[, ! grepl(pattern = "energy_t", x = names(data))]

data_new = data
names(data_new) = make.names(names(data_new))

data_new = data_new %>%
  dplyr::select(-X1, -Fence, -Pool.QC, -Misc.Feature, -Alley) %>%
  select_if(is.numeric) %>%
  na.omit()

df_plot = data.frame(x = data_new$SalePrice)
gg1_dist = ggplot(df_plot, aes(x)) +
  geom_histogram(aes(y = stat(density)), color = "white", bins = 40L) +
  stat_function(fun = dnorm, col = "red",
    args = list(mean = mean(df_plot$x), sd = sd(df_plot$x))) +
  xlab("Sale Price") +
  ylab("Density")

df_plot = data.frame(x = log(data_new$SalePrice))
gg2_dist = ggplot(df_plot, aes(x)) +
  geom_histogram(aes(y = stat(density)), color = "white", bins = 40L) +
  stat_function(fun = dnorm, col = "red", args = list(mean = mean(df_plot$x), sd = sd(df_plot$x))) +
  xlab("Log Sale Price") +
  ylab("Density")

task = removeConstantFeatures(makeRegrTask(id = "Ames Housing", data = data_new, target = "SalePrice"))

lrn = makeLearner("regr.lm")
lrn$id = "No Trafo"

lrn_loglm = cpoLogTrafoRegr() %>>% makeLearner("regr.lm")
lrn_loglm$id = "Log Trafo"

mod = train(learner = "regr.lm", task = task)
mod_log = train(learner = lrn_loglm, task = task)

target = data_new$SalePrice
pred_mod = predict(mod, task)$data$response
pred_mod_log = predict(mod_log, task)$data$response

df_plot = data.frame(target, pred_mod, pred_mod_log)

gg1_pred = ggplot(data = df_plot, aes(x = target, y = pred_mod)) +
  geom_point(alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  xlab("Sale Price") +
  ylab("Predicted Sale Price")

gg2_pred = ggplot(data = df_plot, aes(x = target, y = pred_mod_log)) +
  geom_point(alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  xlab("Sale Price") +
  ylab("exp(Predicted Log Sale Price)")

# Save first plot
library(gridExtra)
pdf("../figure/01_target_original_distribution.pdf", width = 8, height = 4)
grid.arrange(gg1_dist, gg1_pred, ncol = 2)
dev.off()

# Save second plot
pdf("../figure/01_target_log_distribution.pdf", width = 8, height = 4)
grid.arrange(gg2_dist, gg2_pred, ncol = 2)
dev.off()

set.seed(31415)
rdesc = makeResampleInstance(desc = cv10, task = task)
bmr = benchmark(learners = list(lrn, lrn_loglm), tasks = task, resamplings = rdesc, measures = mae)
plot1 = plotBMRBoxplots(bmr, pretty.names = FALSE)
ggsave("../figure/01_transformation_comparison_algorithms.png", plot1, width = 8, height = 4)

lrn_ranger = makeLearner("regr.ranger", num.trees = 200L, mtry = 3L)
lrn_ranger$id = "RF No Trafo"
lrn_logranger = cpoLogTrafoRegr() %>>% lrn_ranger
lrn_logranger$id = "RF Log Trafo"

bmr = benchmark(learners = list(lrn, lrn_loglm, lrn_ranger, lrn_logranger), tasks = task, resamplings = rdesc, measures = mae)
plot2 = plotBMRBoxplots(bmr, pretty.names = FALSE)
ggsave("../figure/01_transformation_comparison_all_algorithms.png", plot2, width = 8, height = 4)
