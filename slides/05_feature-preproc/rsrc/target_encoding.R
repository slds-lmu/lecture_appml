# R Script for Target/Impact Encoding
# Extracted from slides_categ_encode_impact.Rmd

# Setup and libraries
library(knitr)
library(dplyr)
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

# Load data
data = read.csv(paste0(root, "/data/ames_housing_extended.csv"), stringsAsFactors = TRUE)

# Target encoding example - Foundation feature
foundation_counts <- data %>%
  filter(!is.na(Foundation)) %>%
  select(Foundation) %>%
  group_by(Foundation) %>%
  tally()

print("Foundation level counts:")
print(foundation_counts)

# Wood foundation example
wood_examples <- data %>%
  mutate(house.id = row_number()) %>%
  select(house.id, SalePrice, Foundation) %>%
  filter(Foundation == "Wood")

print("Wood foundation examples:")
print(wood_examples)

# Calculate mean price for Wood foundation
wood_mean <- mean(wood_examples$SalePrice, na.rm = TRUE)
print(paste("Wood foundation mean price:", round(wood_mean, 0)))

# Target encoding for all foundation types
foundation_encoding <- data %>%
  select(SalePrice, Foundation) %>%
  group_by(Foundation) %>%
  dplyr::summarize(`Foundation(enc)` = mean(SalePrice, na.rm = TRUE), .groups = 'drop')

print("Foundation target encoding:")
print(foundation_encoding)

# Create visualization of target encoding
foundation_encoding_plot <- foundation_encoding %>%
  ggplot(aes(x = Foundation, y = `Foundation(enc)`)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Foundation Type") +
  ylab("Encoded Value (Mean Sale Price)") +
  ggtitle("Target Encoding for Foundation Feature")

# Save plot
ggsave("../figure/target_encoding_example.pdf", plot = foundation_encoding_plot, width = 8, height = 4)

# Regularized target encoding example
calculate_regularized_encoding <- function(data, feature, target, epsilon = 10) {
  # Calculate global mean
  global_mean <- mean(data[[target]], na.rm = TRUE)
  
  # Calculate encoding for each level
  encoding <- data %>%
    group_by(across(all_of(feature))) %>%
    summarize(
      level_mean = mean(.data[[target]], na.rm = TRUE),
      n_obs = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      lambda = .data$n_obs / (.data$n_obs + epsilon),
      regularized_encoding = .data$lambda * .data$level_mean + (1 - .data$lambda) * global_mean
    )
  
  return(encoding)
}

# Example of regularized encoding
regularized_foundation <- calculate_regularized_encoding(data, "Foundation", "SalePrice", epsilon = 10)

print("Regularized foundation encoding:")
print(regularized_foundation)

# Create comparison plot
comparison_data <- foundation_encoding %>%
  left_join(regularized_foundation, by = "Foundation")

# Create separate data frames for plotting
original_data <- data.frame(
  Foundation = comparison_data$Foundation,
  Encoding = comparison_data$`Foundation(enc)`,
  Method = "Original"
)

regularized_data <- data.frame(
  Foundation = comparison_data$Foundation,
  Encoding = comparison_data$regularized_encoding,
  Method = "Regularized"
)

# Combine manually instead of using tidyr::gather
comparison_plot_data <- rbind(original_data, regularized_data)

regularized_comparison_plot <- comparison_plot_data %>%
  ggplot(aes(x = Foundation, y = Encoding, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Foundation Type") +
  ylab("Encoded Value") +
  ggtitle("Original vs Regularized Target Encoding") +
  scale_fill_manual(values = c("Original" = "steelblue", "Regularized" = "orange"))

# Save comparison plot
ggsave("../figure/regularized_target_encoding.pdf", plot = regularized_comparison_plot, width = 8, height = 4)

print("Target encoding script completed successfully!")
