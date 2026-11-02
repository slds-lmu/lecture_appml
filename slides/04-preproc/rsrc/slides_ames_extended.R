# enable caching for this document
knitr::opts_chunk$set(cache = TRUE)
library(magrittr)
library(ggplot2)
library(tibble)

root = rprojroot::find_root(rprojroot::is_git_root)

data = read.csv(paste0(root, "/data/ames_housing_extended.csv"))

plot1 = ggplot(data, aes(x = SalePrice)) + geom_histogram() + theme_minimal()
ggsave("../figure/ames_target_distribution.pdf", plot1, height = 5.5)
