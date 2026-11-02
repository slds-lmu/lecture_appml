library(knitr)
library(dplyr)
library(mlr)
library(tidyverse)
library(mlrCPO)
library(kableExtra)
library(gridExtra)
library(grid)
root = rprojroot::find_root(rprojroot::is_git_root)

data = read.csv(paste0(root, "/data/ames_housing_extended.csv"), stringsAsFactors = TRUE)

# Generate categorical original table
original_data = data %>%
  select(SalePrice, Central.Air, Bldg.Type) %>%
  slice(5:9)

# Create table plot and save as PDF
pdf("../figure/categorical_original_table.pdf", width = 8, height = 3)
grid.table(original_data)
dev.off()

# Generate one-hot encoded table
onehot_data = data %>%
  select(SalePrice, Central.Air, Bldg.Type) %>%
  slice(5:9) %>%
  createDummyFeatures(target = "SalePrice")

# Create one-hot table plot and save as PDF
pdf("../figure/categorical_onehot_table.pdf", width = 12, height = 3)
grid.table(onehot_data)
dev.off()

task = data %>%
  select(SalePrice, MS.Zoning, Street, Lot.Shape, Land.Contour, Bldg.Type) %>%
  makeRegrTask(id = "None", target = "SalePrice") %>>% cpoFixFactors()

task1 = createDummyFeatures(task, method = "1-of-n")
task1$task.desc$id = "One-Hot"

task2 = createDummyFeatures(task, method = "reference")
task2$task.desc$id = "Dummy"

lrns =  list(
  makeLearner(id = "Linear Regression", "regr.lm"),
  makeLearner(id = "Random Forest", "regr.ranger"))

set.seed(1)
rin = makeResampleInstance(cv10, task1)

res = benchmark(lrns, list(task1, task2, task), rin, mae)
plot1 = as.data.frame(res) %>%
  filter(task.id != "None" | learner.id != "Linear Regression") %>%
  ggplot(aes(y = mae, x = task.id)) + geom_boxplot() + facet_wrap(~learner.id) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  ylab("Mean Absolute Error") +
  xlab("") + ggtitle("Ames House Price Prediction")

ggsave("../figure/categ_dummy_encoding_comparison.png", plot1, height = 4.5)
