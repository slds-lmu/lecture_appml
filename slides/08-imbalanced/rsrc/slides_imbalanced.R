## ----include=FALSE, cache=FALSE, warning=FALSE, message=FALSE--------------------------------------------------
ap = adjust_path(paste0(getwd(), "/figure"))
library(ggplot2)
library(mlr)
library(stats)


## ----echo=FALSE, out.width="0.7\\textwidth", fig.height=2.5, fig.width=6, include = FALSE----------------------
df = rbind(
  data.frame(class = rep("Fraud", 200)),
  data.frame(class = rep("No Fraud", 200000))
  )

p1 = ggplot(data = df) + stat_count(aes(x = class, fill = class)) + ggtitle("Class distribution") + xlab("") + ylim(c(0, 200000)) + theme(legend.position = "none")

## ----echo=FALSE, out.width="0.7\\textwidth", fig.height=2.5, fig.width=5---------------------------------------
# we artificially create an imbalanced setting by throwing away data

set.seed(123456789)
d = getTaskData(spam.task)
levels(d$type) = c("neg", "pos")
ids = sample(which(d$type == "pos"), 0.05*sum(d$type == "neg"))
subset = c(which(d$type == "neg"), which(d$type == "pos")[ids])
task = task.imbalanced = makeClassifTask(data = d[subset,], target = "type")
n = getTaskSize(task)

set.seed(123456789)
test.set = sample(n, size = 0.2 * n)
train.set = setdiff(1:n, test.set)

task.train = subsetTask(task, train.set)
task.test = subsetTask(task, test.set)

lrn = makeLearner("classif.rpart", predict.type = "prob")
mod = train(lrn, task.train)
pred = predict(mod, task.test)
perf = mlr::performance(pred, measures = list(mlr::acc, mlr::bac, mlr::tpr, mlr::fpr, mlr::auc))
perf


p1 = ggplot(data = d[subset,]) + stat_count(aes(x = type, fill = type)) + ggtitle("Class distribution") + xlab("") + theme(legend.position = "none")
p1
ggsave(filename = "figure/unnamed-chunk-3-1.pdf", plot = p1)

calculateConfusionMatrix(pred)


## ----echo=FALSE, out.width="0.7\\textwidth", fig.height=2.5, fig.width=5---------------------------------------
idx = c(2, 9, 16, 23, 51:63)
df = iris[idx, c(1, 2, 5)]
levels(df$Species) = c("pos", "neg", "bla")
colnames(df) = c("x1", "x2", "y")

p1 = ggplot() + geom_histogram(data = df, aes(x = y, fill = y), stat = "count") + theme(legend.position = "none")

min.class = which(df$y == "pos")
resampled = sample(min.class, length(min.class), replace = TRUE)
df2 = rbind(df, df[resampled, ])

p2 = ggplot() + geom_histogram(data = df2, aes(x = y, fill = y), stat = "count") + theme(legend.position = "none")

library(gridExtra)
p = grid.arrange(p1, p2, ncol = 2)
p

ggsave(filename = "figure/unnamed-chunk-4-1.pdf", plot = p)

## ----echo=FALSE, out.width="0.7\\textwidth", fig.height=2.5, fig.width=5---------------------------------------
p1 = ggplot()
p1 = p1 + geom_point(data = df, aes(x = x1, y = x2, colour = y), size = 2)
p1 = p1 + theme(legend.position = "none")

p2 = ggplot() + geom_histogram(data = df, aes(x = y, fill = y), stat = "count") + theme(legend.position = "none")  + ylim(c(0, 15))

p = grid.arrange(p1, p2, nrow = 1)
p

ggsave(filename = "figure/unnamed-chunk-5-1.pdf", plot = p)

## ----echo=FALSE, out.width="0.7\\textwidth", fig.height=2.5, fig.width=5---------------------------------------
task = makeClassifTask(data = df, target = "y")
task.under = undersample(task, rate = 1 / 2)

p1 = ggplot() + geom_point(data = getTaskData(task.under),
	aes(x = x1, y = x2, colour = y), size = 2) + theme(legend.position = "none")

p2 = ggplot() + geom_histogram(data = getTaskData(task.under), aes(x = y, fill = y), stat = "count") + theme(legend.position = "none") + ylim(c(0, 15))

p = grid.arrange(p1, p2, nrow = 1)

ggsave(filename = "figure/unnamed-chunk-6-1.pdf", plot = p)

## ----echo=FALSE, fig.height=3.5, include = FALSE---------------------------------------------------------------
data = rbind(
  data.frame(x = rnorm(100, mean = 1), class = "spam"),
  data.frame(x = rnorm(5000, mean = 2), class = "no spam")
	)

task = makeClassifTask(data = data, target = "class")
task.over = oversample(task, rate = 8)
task.under = undersample(task, rate = 1/8)

p1 = ggplot() + stat_count(aes(x = getTaskTargets(task))) + ggtitle("Original") + xlab("") + ylim(c(0, 5000))
p2 = ggplot() + stat_count(aes(x = getTaskTargets(task.over))) + ggtitle("Oversampling") + xlab("") + ylim(c(0, 5000))
p3 = ggplot() + stat_count(aes(x = getTaskTargets(task.under))) + ggtitle("Undersampling") + xlab("") + ylim(c(0, 5000))
grid.arrange(p1, p2, p3, nrow = 1)


## ----echo=FALSE, out.width="0.7\\textwidth", fig.height=2.5, fig.width=5---------------------------------------

p1 = ggplot()
p1 = p1 + geom_point(data = df, aes(x = x1, y = x2, colour = y), size = 2)
p1 = p1 + theme(legend.position = "none")

min.class  =which(df$y == "pos")
grid = as.data.frame(t(combn(x = min.class, 2)))
df.segm = cbind(df[grid$V1, 1:2], df[grid$V2, 1:2])
names(df.segm) = c("x", "y", "xend", "yend")

p3 = p1 + geom_segment(data = df.segm[c(1,3), ], aes(x = x, y = y, xend = xend, yend = yend), alpha = 0.5)
p3 = p3 + geom_point(data = df[1, ], aes(x = x1, y = x2, colour = y), size = 4)
p3 = p3 + theme(legend.position = "none")

p2 = ggplot() + geom_histogram(data = df, aes(x = y, fill = y), stat = "count") + theme(legend.position = "none")

p = grid.arrange(p3, p2, nrow = 1)

ggsave(filename = "figure/unnamed-chunk-8-1.pdf", plot = p)

## ----echo=FALSE, out.width="0.7\\textwidth", fig.height=2.5, fig.width=5---------------------------------------
task = makeClassifTask(data = df, target = "y")

set.seed(1234)
task.smote = smote(task, rate = 3, nn = 2)
smote.data = getTaskData(task.smote)

p3 = p3 + geom_point(data = smote.data[20, ],
	aes(x = x1, y = x2, colour = y), size = 3)
p2 = ggplot() + geom_histogram(data = rbind(df, smote.data[20, ]), aes(x = y, fill = y), stat = "count") + theme(legend.position = "none")

p = grid.arrange(p3, p2, nrow = 1)

ggsave(filename = "figure/unnamed-chunk-9-1.pdf", plot = p)

## ----echo=FALSE, out.width="0.7\\textwidth", fig.height=2.5, fig.width=5---------------------------------------
p1 = p1 + geom_segment(data = df.segm[c(1:3, 5:6), ], aes(x = x, y = y, xend = xend, yend = yend), alpha = 0.5)
p1 = p1 + theme(legend.position = "none")
p1 = p1 + geom_point(data = getTaskData(task.smote),
	aes(x = x1, y = x2, colour = y), size = 2)

p2 = ggplot() + geom_histogram(data = smote.data, aes(x = y, fill = y), stat = "count") + theme(legend.position = "none")

p = grid.arrange(p1, p2, nrow = 1)

ggsave(filename = "figure/unnamed-chunk-10-1.pdf", plot = p)

## ----echo=FALSE, out.width="0.7\\textwidth", fig.height=2.5, fig.width=5---------------------------------------
set.seed(123456789)

types = getTaskData(spam.task)$type
#subset = c(which(types == "nonspam"), which(types == "spam")[1:50])
task = task.imbalanced
n = getTaskSize(task)

test.set = sample(n, size = 0.2 * n)
train.set = setdiff(1:n, test.set)

task.train = subsetTask(task, train.set)
task.test = subsetTask(task, test.set)
task.under = undersample(task.train, rate =  1 / 20)
task.over = oversample(task.train, rate = 20)
task.smote = smote(task.train, rate = 20, nn = 5)
tasks = list("imbalanced" = task.train, "oversampling" = task.over, "undersampling" = task.under, "SMOTE" = task.smote)

lrn = makeLearner("classif.rpart", predict.type = "prob")
perf = data.frame()

for (i in 1:length(tasks)) {
	mod = train(lrn, tasks[[i]])
	pred = predict(mod, task.test)
	perf = rbind(perf, mlr::performance(pred, measures = list(mlr::acc, mlr::bac, mlr::tpr, mlr::fpr, mlr::auc)))
}
names(perf) = c("acc", "bac", "tpr", "fpr", "auc")
perf$method = names(tasks)
perf

