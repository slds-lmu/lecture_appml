# R Script for Ames Housing Dataset Overview
# Extracted from slides_ames_extended.Rmd

# Setup and libraries
library(ggplot2)
library(magrittr)
library(tibble)
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

# Load Ames housing data
data = read.csv(paste0(root, "/data/ames_housing_extended.csv"))

# Create histogram of sale prices
ames_target_plot <- ggplot(data, aes(x = SalePrice)) + 
  geom_histogram(bins = 40, fill = "steelblue", color = "white") + 
  theme_minimal() +
  xlab("Sale Price") +
  ylab("Frequency") +
  ggtitle("Distribution of Sale Prices in Ames Housing Dataset")

# Save plot
ggsave("../figure/ames_target_distribution.pdf", plot = ames_target_plot, width = 8, height = 5.5)

# Print dataset summary information
cat("Ames Housing Dataset Summary:\n")
cat("Number of observations:", nrow(data), "\n")
cat("Number of features:", ncol(data) - 1, "\n")  # Excluding target

# Count different feature types
numeric_features <- sapply(data, is.numeric)
factor_features <- sapply(data, is.factor)
character_features <- sapply(data, is.character)

cat("Numeric features:", sum(numeric_features), "\n")
cat("Factor features:", sum(factor_features), "\n") 
cat("Character features:", sum(character_features), "\n")

print("Ames housing dataset overview script completed successfully!")
