# R Script for Model-Based Imputation
# Extracted from slides_03_imputation_models.Rmd

# Setup and libraries
library(ggplot2)
library(rprojroot)
library(readr)

# Define helper function
adjust_path <- function(path) {
  return(path)
}

root = rprojroot::find_root(rprojroot::is_git_root)
ap = adjust_path(paste0(getwd(), "/figure"))

data = readr::read_csv(paste0(root, "/data/ames_housing_extended.csv"))
colnames(data)[1] = "X1"
data = data[, ! grepl(pattern = "energy_t", x = names(data))]

# Model-Based Imputation: Drawbacks - Surrogate model influence
set.seed(618)
x = runif(n = 100, min = 0, max = 10)
y = 2 + 2 * x * sin(x) + 2 * x + rnorm(100, mean = 0, sd = 2)

mod = lm(y ~ x)
x_new = c(2, 5, 8, 8.5, 9)
pred_new = predict(mod, newdata = data.frame(x = x_new))

df_plot = data.frame(x = x, y = y)
df_new_points = data.frame(x = x_new, y = pred_new)

# Generate surrogate model influence plot
surrogate_plot <- ggplot() +
  geom_point(data = df_plot, aes(x = x, y  = y)) +
  geom_smooth(data = df_plot, aes(x = x, y = y), method = "lm", se = FALSE,
    color = "red") +
  geom_point(data = df_new_points, aes(x = x, y = y), color = "red", size = 3) +
  xlab("Feature used for imputation") +
  ylab("Feature to impute")

# Save plot
ggsave("../figure/surrogate_model_influence.pdf", plot = surrogate_plot, width = 4, height = 3)
