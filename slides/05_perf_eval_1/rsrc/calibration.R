# Load required packages
library(ggplot2)
library(dplyr)
library(glmnet)
library(isotone)

# Generate synthetic data
set.seed(1)
n <- 200
x <- seq(0, 1, length.out = n)
y <- floor((as.numeric(x + rnorm(n, sd = 0.1) > 0) + rbinom(n, 1, x))/2)

# Create bins using deciles
bins <- quantile(x, probs = seq(0, 1, by = 0.05))
bin_indices <- cut(x, breaks = bins, include.lowest = TRUE, labels = FALSE)
bin_centers <- sapply(split(x, bin_indices), mean)
bin_means <- sapply(split(y, bin_indices), mean)

# Data frame for empirical binning
df_bins <- data.frame(bin_centers, bin_means)


# Function to perform empirical binning based on quantile bins and return a calibrated score
empirical_binning <- function(train_scores, train_labels, test_score, n_bins = 20) {
  # Create bins using quantiles (deciles by default)
  bins <- quantile(train_scores, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
  bin_indices <- cut(train_scores, breaks = bins, include.lowest = TRUE, labels = FALSE)
  
  # Calculate mean proportion of positive instances for each bin
  bin_means <- sapply(split(train_labels, bin_indices), mean, na.rm = TRUE)
  
  # Function to find the appropriate bin and return the corresponding mean
  find_bin_mean <- function(score) {
    bin <- findInterval(score, bins, rightmost.closed = TRUE)
    return(bin_means[bin])
  }
  
  # Return the calibrated score for the test score
  calibrated_score <- find_bin_mean(test_score)
  return(calibrated_score)
}
# Empirical Binning
#emp_bin <- empirical_binning(x, y, x)
x_emp_bin <- seq(0.01, 0.99, length.out = 100)
y_emp_bin <- empirical_binning(x, y, x_emp_bin)

# Platt Scaling (Logistic Regression)
log_reg <- glm(y ~ x, family = "binomial")
x_log_reg <- seq(0.01, 0.99, length.out = 100)
y_log_reg <- predict(log_reg, newdata = data.frame(x = x_log_reg), type = "response")

# Isotonic Regression
iso_reg <- isoreg(x, y)
x_iso_reg <- seq(0.01, 0.99, length.out = 100)
y_iso_reg <- as.stepfun(iso_reg)(x_iso_reg) #predict(iso_reg, newdata = x_iso_reg)

# Data frame for plotting logistic and isotonic regression
df_emp_bin <- data.frame(x_emp_bin, y_emp_bin)
df_log_reg <- data.frame(x_log_reg, y_log_reg)
df_iso_reg <- data.frame(x_iso_reg, y_iso_reg)

# Plotting
base = ggplot(df_bins, aes(x = bin_centers, y = bin_means)) +
  geom_point(color = "blue") +
  geom_point(data = data.frame(x, y), aes(x, y), color = "blue", pch = 4) 

p1 <- base +
  geom_line(data = df_emp_bin, aes(x = x_emp_bin, y = y_emp_bin), color = "red", size = 1) +
  #geom_segment(aes(xend = bin_centers, yend = bin_means), color = "red", size = 1) +
  labs(title = "Empirical Binning", x = "Predicted Score", y = "Proportion of Positive Instances") +
  theme_minimal()

p2 <- base +
  geom_line(data = df_log_reg, aes(x = x_log_reg, y = y_log_reg), color = "red", size = 1) +
  labs(title = "Platt Scaling", x = "Predicted Score", y = "Proportion of Positive Instances") +
  theme_minimal()

p3 <- base +
  geom_line(data = df_iso_reg, aes(x = x_iso_reg, y = y_iso_reg), color = "red", size = 1) +
  labs(title = "Isotonic Regression", x = "Predicted Score", y = "Proportion of Positive Instances") +
  theme_minimal()

# Arrange plots
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
