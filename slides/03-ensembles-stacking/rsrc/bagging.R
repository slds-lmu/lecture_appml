library(mlr3)
library(mlr3pipelines)
library(mlr3misc)
library(ggplot2)
library(cowplot)

# inspired from https://scikit-learn.org/stable/auto_examples/ensemble/plot_bias_variance.html

n_repeat = 50
n_train = 50
n_test = 1000
noise = 0.1  # sd, so y_noise will later be noise ^ 2

set.seed(1)

f = function(x) {
  exp(-(x^2)) + 1.5 * (exp(-((x - 2)^2)))
}

generate_data = function(n_samples, noise, n_repeat = 1) {
  x = runif(n_samples, min = -5, max = 5)
  x = sort(x)
  if (n_repeat == 1) {
    y = f(x) + rnorm(n_samples, mean = 0, sd = noise)
  } else {
    y = map(seq_len(n_repeat), function(i) {
      f(x) + rnorm(n_samples, mean = 0, sd = noise)
    })
  }
  list(x = x, y = y)
}

tree = lrn("regr.rpart", minsplit = 5)
tree$id = "tree"

bagging100 = GraphLearner$new(ppl("greplicate",
  po("subsample") %>>%
  po("learner", lrn("regr.rpart", minsplit = 5)),
  n = 100
) %>>%
  po("regravg"),
  id = "bagging10"
)

tasks = map(seq_len(n_repeat), function(repl) {
  dat = generate_data(n_train, noise = noise)
  TaskRegr$new(paste0("train_", repl), target = "y", backend = as.data.frame(do.call(cbind, dat)))
})

test_data = generate_data(n_test, noise = noise, n_repeat = n_repeat)

x_test = test_data$x
y_test = do.call(cbind, test_data$y)

results = map(list(tree, bagging100), function(learner) {
 
  y_predict = matrix(0, nrow = n_test, ncol = n_repeat)

  for (i in seq_len(n_repeat)) {
    learner$train(tasks[[i]])
    y_predict[, i] = learner$predict_newdata(data.frame(x = x_test))$response
  }

  y_error = numeric(n_test)


  for (i in seq_len(n_repeat)) {
    for (j in seq_len(n_repeat)) {
      y_error = y_error + (y_test[, j] - y_predict[, i]) ^ 2
    }
  }

  y_error = y_error / (n_repeat * n_repeat)
  y_noise = apply(y_test, MARGIN = 1L, FUN = var)
  y_bias = (f(x_test) - apply(y_predict, MARGIN = 1L, FUN = mean)) ^ 2
  y_var = apply(y_predict, MARGIN = 1L, FUN = var)
  
  
  list(learner_id = learner$id, y_error = y_error, y_noise = y_noise, y_bias = y_bias, y_var = y_var, y_predict = y_predict)
})


# FIXME: automate, prettify labels and legends, ...

p_top = ggplot() +
  geom_line(data = data.table(x = x_test, y = f(x_test)), aes(x = x, y = y, colour = "f(x)")) +
  geom_point(data = data.table(x = tasks[[1]]$data(cols = "x")[[1]], y = tasks[[1]]$data(cols = "y")[[1]]), aes(x = x, y = y, colour = "y ~ f(x) + e")) + 
  geom_step(data = data.table(x = x_test, y = results[[1]]$y_predict[, 1]), aes(x = x, y = y, colour = "y^(x)")) +
  geom_step(data = data.table(x = x_test, y = apply(results[[1]]$y_predict, MARGIN = 1L, FUN = mean)), aes(x = x, y = y, colour = "E[y^(x)]"))


for (i in seq_len(n_repeat)[-1]) {
  p_top = p_top + geom_step(data = data.table(x = x_test, y = results[[1]]$y_predict[, i]), aes(x = x, y = y), colour = "red", alpha = 0.05)
}

p_top = p_top +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "x", y = "", title = "Tree") +
  scale_colour_manual(name = "",
    values = c("f(x)" = "black", "y ~ f(x) + e" = "blue", "y^(x)" = "red", "E[y^(x)]" = "mediumturquoise"))

p_bottom = ggplot() +
  geom_line(data = data.table(x = x_test, y = results[[1]]$y_error), aes(x = x, y = y, colour = "error(x)")) +
  geom_line(data = data.table(x = x_test, y = results[[1]]$y_bias), aes(x = x, y = y, colour = "bias(x)^2")) +
  geom_line(data = data.table(x = x_test, y = results[[1]]$y_var), aes(x = x, y = y, colour = "variance(x)")) +
  geom_line(data = data.table(x = x_test, y = results[[1]]$y_noise), aes(x = x, y = y, colour = "noise(x)"))

p_bottom = p_bottom +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "x", y = "") +
  ylim(c(0, 0.15)) +
  scale_colour_manual(name = "",
    values = c("error(x)" = "red", "bias(x)^2" = "blue", "variance(x)" = "darkgreen", "noise(x)" = "mediumturquoise"))

p = plot_grid(p_top, p_bottom, nrow = 2, ncol = 1)

ggsave("../figure_man/bagging_variance_bias_tree.png")

p_top = ggplot() +
  geom_line(data = data.table(x = x_test, y = f(x_test)), aes(x = x, y = y, colour = "f(x)")) +
  geom_point(data = data.table(x = tasks[[1]]$data(cols = "x")[[1]], y = tasks[[1]]$data(cols = "y")[[1]]), aes(x = x, y = y, colour = "y ~ f(x) + e")) + 
  geom_step(data = data.table(x = x_test, y = results[[2]]$y_predict[, 1]), aes(x = x, y = y, colour = "y^(x)")) +
  geom_step(data = data.table(x = x_test, y = apply(results[[2]]$y_predict, MARGIN = 1L, FUN = mean)), aes(x = x, y = y, colour = "E[y^(x)]"))


for (i in seq_len(n_repeat)[-1]) {
  p_top = p_top + geom_step(data = data.table(x = x_test, y = results[[2]]$y_predict[, i]), aes(x = x, y = y), colour = "red", alpha = 0.05)
}

p_top = p_top +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "x", y = "", title = "Bagging 100 Trees") +
  scale_colour_manual(name = "",
    values = c("f(x)" = "black", "y ~ f(x) + e" = "blue", "y^(x)" = "red", "E[y^(x)]" = "mediumturquoise"))

p_bottom = ggplot() +
  geom_line(data = data.table(x = x_test, y = results[[2]]$y_error), aes(x = x, y = y, colour = "error(x)")) +
  geom_line(data = data.table(x = x_test, y = results[[2]]$y_bias), aes(x = x, y = y, colour = "bias(x)^2")) +
  geom_line(data = data.table(x = x_test, y = results[[2]]$y_var), aes(x = x, y = y, colour = "variance(x)")) +
  geom_line(data = data.table(x = x_test, y = results[[2]]$y_noise), aes(x = x, y = y, colour = "noise(x)"))

p_bottom = p_bottom +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "x", y = "") +
  ylim(c(0, 0.15)) +
  scale_colour_manual(name = "",
    values = c("error(x)" = "red", "bias(x)^2" = "blue", "variance(x)" = "darkgreen", "noise(x)" = "mediumturquoise"))

p = plot_grid(p_top, p_bottom, nrow = 2, ncol = 1)

ggsave("../figure_man/bagging_variance_bias_100_trees.png")

