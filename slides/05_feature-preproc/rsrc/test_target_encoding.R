# Test script for target encoding plots
library(ggplot2)
library(dplyr)

# Create simple test data
set.seed(123)
n <- 1000
test_data <- data.frame(
  Foundation = sample(c("BrkTil", "CBlock", "PConc", "Slab", "Stone", "Wood"), n, replace = TRUE),
  SalePrice = rnorm(n, mean = 200000, sd = 50000)
)

# Make prices somewhat dependent on foundation type for realistic encoding
price_adjustments <- c("BrkTil" = -20000, "CBlock" = -10000, "PConc" = 0, 
                      "Slab" = -15000, "Stone" = 5000, "Wood" = -25000)
test_data$SalePrice <- test_data$SalePrice + price_adjustments[test_data$Foundation]

# Target encoding for all foundation types
foundation_encoding <- test_data %>%
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

# Create regularized encoding example
calculate_regularized_encoding <- function(data, feature, target, epsilon = 10) {
  global_mean <- mean(data[[target]], na.rm = TRUE)
  
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

regularized_foundation <- calculate_regularized_encoding(test_data, "Foundation", "SalePrice", epsilon = 10)

# Create comparison data manually
original_data <- data.frame(
  Foundation = foundation_encoding$Foundation,
  Encoding = foundation_encoding$`Foundation(enc)`,
  Method = "Original"
)

regularized_data <- data.frame(
  Foundation = regularized_foundation$Foundation,
  Encoding = regularized_foundation$regularized_encoding,
  Method = "Regularized"
)

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

print("Test target encoding script completed successfully!")
