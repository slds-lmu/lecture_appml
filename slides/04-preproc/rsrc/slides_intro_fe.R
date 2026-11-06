library(knitr)
library(ggplot2)
root = rprojroot::find_root(rprojroot::is_git_root)

d = data.frame(
  Method = factor(1:4, labels = c("Linear Regression", "Gradient Boosting", "Linear Regression w. Feat. Eng.", "Gradient Boosting w. Feat. Eng.")),
  Error = c(25.5, 10, 11, 9.8)
)

plot1 = ggplot(data = d) + geom_bar(aes(x = Method, y = Error), stat = "identity") + theme_minimal() + theme(axis.text.x = element_text(angle = 15, hjust = 1))
ggsave("../figure/intro_fe_importance.png", plot1, height = 4.5)
