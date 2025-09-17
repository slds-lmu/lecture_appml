library(mlr)
library(mlrCPO)
library(ggplot2)
library(dplyr)

root = rprojroot::find_root(rprojroot::is_git_root)

data = readr::read_csv(paste0(root, "/data/ames_housing_extended.csv"))
colnames(data)[1] = "X1"
data = data[, ! grepl(pattern = "energy_t", x = names(data))]

data_new = data
names(data_new) = make.names(names(data_new))

task = data_new %>%
  dplyr::select(-X1, -Fence, -Pool.QC, -Misc.Feature, -Alley) %>%
  select_if(is.numeric) %>%
  na.omit() %>%
  makeRegrTask(id = "Ames Housing", data = ., target = "SalePrice") %>%
  removeConstantFeatures()

lrn_kknn_no_scale = makeLearner("regr.kknn", scale = FALSE)
lrn_kknn_no_scale$id = "No Scaling"
lrn_kknn_scale = mlrCPO::cpoScale() %>>% makeLearner("regr.kknn", scale = FALSE)
lrn_kknn_scale$id = "Normalize Features"
lrn_kknn_boxcox = makePreprocWrapperCaret(lrn_kknn_no_scale, ppc.BoxCox = TRUE)
lrn_kknn_boxcox$id = "Box-Cox Trafo"

set.seed(31415)
bmr = benchmark(learners = list(lrn_kknn_no_scale, lrn_kknn_scale, lrn_kknn_boxcox),
  tasks = task, resamplings = cv10, measures = mae)
plot1 = plotBMRBoxplots(bmr, pretty.names = FALSE)
ggsave("../figure/02_feature_transformation_knn.png", plot1, width = 8, height = 4)
