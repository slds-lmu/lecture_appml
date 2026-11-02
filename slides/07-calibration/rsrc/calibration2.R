# Load required packages
library(dplyr)
library(isotone)
library(glmnet)

### Empirical Binning Function
empirical_binning <- function(train_scores, train_labels, test_scores, n_bins = 10) {
  # Create bins using quantiles
  bins <- quantile(train_scores, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
  bin_indices <- cut(train_scores, breaks = bins, include.lowest = TRUE, labels = FALSE)
  
  # Calculate mean proportion of positive instances for each bin
  bin_means <- sapply(split(train_labels, bin_indices), mean, na.rm = TRUE)
  
  # Function to find the appropriate bin and return the corresponding mean
  find_bin_mean <- function(score) {
    bin <- findInterval(score, bins, rightmost.closed = TRUE)
    return(bin_means[bin])
  }
  
  # Return the calibrated scores for the test scores
  calibrated_scores <- sapply(test_scores, find_bin_mean)
  return(calibrated_scores)
}

### Isotonic Regression Function
isotonic_regression <- function(train_scores, train_labels, test_scores) {
  # Fit isotonic regression model
  iso_fit <- isoreg(train_scores, train_labels)
  iso_fit <- as.stepfun(iso_fit)
  # Predict calibrated scores for test data
  calibrated_scores <- iso_fit(test_scores)
  return(calibrated_scores)
}

### Platt Scaling Function
platt_scaling <- function(train_scores, train_labels, test_scores) {
  # Fit logistic regression model (Platt scaling)
  log_reg <- glm(train_labels ~ train_scores, family = binomial)
  
  # Predict calibrated probabilities for test data
  test_data <- data.frame(train_scores = test_scores)
  calibrated_scores <- predict(log_reg, newdata = test_data, type = "response")
  return(calibrated_scores)
}

# Example usage:
# Generate synthetic data
set.seed(1)
n <- 400
x <- runif(n)#seq(0, 1, length.out = n)
y <- floor((as.numeric(x + rnorm(n, sd = 0.1) > 0) + rbinom(n, 1, x))/2)

# Create bins using deciles
bins <- quantile(x, probs = seq(0, 1, by = 0.1))
bin_indices <- cut(x, breaks = bins, include.lowest = TRUE, labels = FALSE)
bin_centers <- sapply(split(x, bin_indices), mean)
bin_means <- sapply(split(y, bin_indices), mean)

# Data frame for empirical binning
df_bins <- data.frame(bin_centers, bin_means)


# Calibrate the test scores using the three methods
calibrated_scores_empirical <- empirical_binning(x, y, x)
calibrated_scores_isotonic <- isotonic_regression(x, y, x)
calibrated_scores_platt <- platt_scaling(x, y, x)

# Data frame for plotting logistic and isotonic regression
df_emp_bin <- data.frame(x, calibrated_scores_empirical)
df_log_reg <- data.frame(x, calibrated_scores_isotonic)
df_iso_reg <- data.frame(x, calibrated_scores_platt)

# Plotting
base = ggplot(df_bins, aes(x = bin_centers, y = bin_means)) +
  geom_point(color = "blue") +
  geom_point(data = data.frame(x, y), aes(x, y), color = "blue", pch = 4) 

p1 <- base +
  geom_line(data = df_emp_bin, aes(x = x, y = calibrated_scores_empirical), color = "red", size = 1) +
  #geom_segment(aes(xend = bin_centers, yend = bin_means), color = "red", size = 1) +
  labs(title = "Empirical Binning", x = "Predicted Score", y = "Proportion of Positive Instances") +
  theme_minimal()

p2 <- base +
  geom_line(data = df_log_reg, aes(x = x, y = calibrated_scores_platt), color = "red", size = 1) +
  labs(title = "Platt Scaling", x = "Predicted Score", y = "Proportion of Positive Instances") +
  theme_minimal()

p3 <- base +
  geom_line(data = df_iso_reg, aes(x = x, y = calibrated_scores_isotonic), color = "red", size = 1) +
  labs(title = "Isotonic Regression", x = "Predicted Score", y = "Proportion of Positive Instances") +
  theme_minimal()

# Arrange plots
library(patchwork)
p = p1 + plot_spacer() + plot_spacer() 
pp = p1 + p2 + plot_spacer() 
ppp = p1 + p2 + p3
ggsave("calibration_methods1.pdf", plot = p, width = 9, height = 3)
ggsave("calibration_methods2.pdf", plot = pp, width = 9, height = 3)
ggsave("calibration_methods3.pdf", plot = ppp, width = 9, height = 3)

