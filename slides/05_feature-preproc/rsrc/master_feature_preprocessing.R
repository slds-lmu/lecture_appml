# Master R Script for Feature Preprocessing Slides
# This script combines all R code from the converted Rmd files

# ==============================================================================
# SETUP AND COMMON LIBRARIES
# ==============================================================================

# Load required libraries
library(ggplot2)
library(dplyr)
library(knitr)
library(rprojroot)
library(readr)
library(magrittr)
library(tibble)

# Install gridExtra if not available
if (!require(gridExtra, quietly = TRUE)) {
  install.packages("gridExtra")
  library(gridExtra)
}

# Install tidyr if not available  
if (!require(tidyr, quietly = TRUE)) {
  install.packages("tidyr")
  library(tidyr)
}

# Setup paths
root <- tryCatch({
  rprojroot::find_root(rprojroot::is_git_root)
}, error = function(e) {
  "../../.."
})

# Define helper function
adjust_path <- function(path) {
  return(path)
}

# ==============================================================================
# SCRIPT 1: FEATURE ENGINEERING INTRODUCTION
# ==============================================================================

cat("Running Feature Engineering Introduction...\n")
source("intro_fe.R")

# ==============================================================================
# SCRIPT 2: TARGET TRANSFORMATION
# ==============================================================================

cat("Running Target Transformation...\n")
source("target_transformation.R")

# ==============================================================================
# SCRIPT 3: FEATURE TRANSFORMATIONS
# ==============================================================================

cat("Running Feature Transformations...\n")
source("feature_transformations.R")

# ==============================================================================
# SCRIPT 4: CATEGORICAL ENCODING - DUMMY/ONE-HOT
# ==============================================================================

cat("Running Categorical Encoding (Dummy)...\n")
source("categorical_encoding_dummy.R")

# ==============================================================================
# SCRIPT 5: TARGET ENCODING
# ==============================================================================

cat("Running Target Encoding...\n")
source("target_encoding.R")

# ==============================================================================
# SCRIPT 6: AMES DATASET OVERVIEW
# ==============================================================================

cat("Running Ames Dataset Overview...\n")
source("ames_dataset.R")

# ==============================================================================
# SCRIPT 7: DATA LEAKAGE CONCEPTS
# ==============================================================================

cat("Running Data Leakage Demonstration...\n")
source("data_leakage.R")

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

# Function to check if all figures were generated
check_generated_figures <- function() {
  figure_dir <- "../figure"
  expected_figures <- c(
    "fe_importance_comparison.pdf",
    "target_distribution_comparison.pdf", 
    "target_prediction_comparison.pdf",
    "target_transformation_benchmark.pdf",
    "feature_scaling_benchmark.pdf",
    "categorical_encoding_comparison.pdf",
    "target_encoding_example.pdf",
    "regularized_target_encoding.pdf",
    "ames_target_distribution.pdf"
  )
  
  existing_figures <- list.files(figure_dir, pattern = "\\.pdf$")
  missing_figures <- setdiff(expected_figures, existing_figures)
  
  if (length(missing_figures) == 0) {
    cat("All expected figures have been generated successfully!\n")
  } else {
    cat("Missing figures:\n")
    cat(paste(missing_figures, collapse = "\n"))
  }
  
  cat("Generated figures:\n")
  cat(paste(existing_figures, collapse = "\n"))
}

# Function to create feature engineering summary
create_fe_summary <- function() {
  cat("\n=== FEATURE ENGINEERING SUMMARY ===\n")
  cat("1. Feature Engineering Introduction - Importance comparison\n")
  cat("2. Target Transformation - Log transformation effects\n") 
  cat("3. Feature Transformations - Scaling effects on k-NN\n")
  cat("4. Categorical Encoding - One-hot vs dummy vs none\n")
  cat("5. Target Encoding - Mean encoding with regularization\n")
  cat("6. Ames Dataset - Target distribution overview\n")
  cat("7. Data Leakage - Proper train-test splitting\n")
  cat("=====================================\n")
}

# Run summary functions
create_fe_summary()
check_generated_figures()

print("All feature preprocessing scripts have been loaded and executed successfully!")
print("Available utility functions:")
print("- check_generated_figures()")
print("- create_fe_summary()")
