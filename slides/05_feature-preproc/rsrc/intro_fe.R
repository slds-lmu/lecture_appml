# R Script for Feature Engineering Introduction
# Extracted from slides_intro_fe.Rmd

# Setup and libraries
library(knitr)
library(ggplot2)
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

# Why Feature Engineering is Important - Bar chart
d = data.frame(
  Method = factor(1:4, labels = c("Linear Regression", "Gradient Boosting", 
                                  "Linear Regression w. Feat. Eng.", 
                                  "Gradient Boosting w. Feat. Eng.")),
  Error = c(25.5, 10, 11, 9.8)
)

# Generate the plot
fe_importance_plot <- ggplot(data = d) + 
  geom_bar(aes(x = Method, y = Error), stat = "identity") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  ylab("Error") +
  xlab("Method")

# Save plot
ggsave("../figure/fe_importance_comparison.pdf", plot = fe_importance_plot, width = 8, height = 4.5)

print("Feature Engineering Introduction script completed successfully!")
