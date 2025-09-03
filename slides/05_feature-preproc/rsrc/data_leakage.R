# R Script for Data Leakage Concepts
# Extracted from slides_leakage.Rmd

# Setup and libraries
library(knitr)
library(rprojroot)

# Define helper function
adjust_path <- function(path) {
  return(path)
}

# Find project root
root <- tryCatch({
  rprojroot::find_root(rprojroot::is_git_root)
}, error = function(e) {
  "../../.."
})

ap = adjust_path(paste0(getwd(), "/figure"))

# Note: This script mainly references external figures for train-test leakage concepts
# The main content is conceptual rather than computational

# Create a simple demonstration of proper train-test split
set.seed(123)
n_samples <- 1000
data_example <- data.frame(
  feature = rnorm(n_samples),
  target = rnorm(n_samples)
)

# Proper way: split first, then scale
train_indices <- sample(1:n_samples, size = 0.7 * n_samples)
train_data <- data_example[train_indices, ]
test_data <- data_example[-train_indices, ]

# Calculate scaling parameters on training data only
train_mean <- mean(train_data$feature)
train_sd <- sd(train_data$feature)

# Apply scaling to both sets using training parameters
train_data$feature_scaled <- (train_data$feature - train_mean) / train_sd
test_data$feature_scaled <- (test_data$feature - train_mean) / train_sd

cat("Proper scaling approach:\n")
cat("Training data feature mean:", round(mean(train_data$feature_scaled), 4), "\n")
cat("Training data feature sd:", round(sd(train_data$feature_scaled), 4), "\n")
cat("Test data feature mean:", round(mean(test_data$feature_scaled), 4), "\n")
cat("Test data feature sd:", round(sd(test_data$feature_scaled), 4), "\n")

# Wrong way (leakage): scale entire dataset first
data_example$feature_scaled_wrong <- scale(data_example$feature)[, 1]
train_data_wrong <- data_example[train_indices, ]
test_data_wrong <- data_example[-train_indices, ]

cat("\nIncorrect scaling approach (with leakage):\n")
cat("Training data feature mean:", round(mean(train_data_wrong$feature_scaled_wrong), 4), "\n")
cat("Training data feature sd:", round(sd(train_data_wrong$feature_scaled_wrong), 4), "\n")
cat("Test data feature mean:", round(mean(test_data_wrong$feature_scaled_wrong), 4), "\n")
cat("Test data feature sd:", round(sd(test_data_wrong$feature_scaled_wrong), 4), "\n")

print("Data leakage demonstration script completed successfully!")
