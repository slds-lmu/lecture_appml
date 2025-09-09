# R Script for Imputation Introduction
# Extracted from slides_01_imputation_intro.Rmd

# Setup and libraries
library(ggplot2)
library(magrittr)
library(tidyverse)
library(rprojroot)

# Define helper function
adjust_path <- function(path) {
  return(path)
}

# Find project root
root <- tryCatch({
  rprojroot::find_root(rprojroot::is_git_root)
}, error = function(e) {
  # Fallback to relative path if rprojroot fails
  "../../.."
})

ap = adjust_path(paste0(getwd(), "/figure"))

# Motivating Example - Missing Values Plot
library(ggplot2)

n_cols = 200L

# Create probability plot for different missing value percentages
df_probs = lapply(c(0.01, 0.02, 0.05, 0.1), FUN = function (p) {
  n_feats = seq_len(n_cols)
  na_prob = 1 - pbinom(q = 0, size = n_feats, p = p)

  return (data.frame(n_feats = n_feats, na_prob = na_prob, prob = p))
})
df_plots = do.call(rbind, df_probs)
df_plots$prob = paste0(df_plots$prob * 100, " %")

# Generate the plot
missing_values_plot <- ggplot(data = df_plots, aes(x = n_feats, y = na_prob, color = prob)) +
  geom_line() +
  xlab("Number of Features") +
  ylab("Percentage of\nNon-Usable Observations") +
  labs(color = "Percentage of missing\nobservations per\nfeature")

# Save plot
ggsave("../figure/missing_values_plot.pdf", plot = missing_values_plot, width = 6, height = 3)

# Visualizing Missing Values
library(naniar)
data = readr::read_csv(paste0(root, "/data/ames_housing_extended.csv"))
colnames(data)[1] = "X1"
data = select(data, -X1, -matches("energy"))

# Generate missing values visualization
missing_vis_plot <- vis_miss(data) + theme(axis.text.x = element_text(angle = 90, size = 10))

# Save plot
ggsave("../figure/missing_values_visualization.pdf", plot = missing_vis_plot, width = 12, height = 6)
