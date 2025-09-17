# Example plots for threshold tuning

set.seed(32)
df = data.frame(x = 1:10, y = c(rep(0, 6), c(1, 0, 1, 1)))
df$y = as.factor(df$y)
task = makeClassifTask(data = df, target = "y", positive = 1)
learner = makeLearner("classif.svm", predict.type = "prob")
mod = train(learner, task)
pred = predict(mod, task)

rocdata = generateThreshVsPerfData(pred, measures =  list(fpr, tpr))
p = plotROCCurves(rocdata)

# A bad choice
d = rocdata$data
id = max(which(d$threshold <= 0.5))
p1 = p + geom_point(data = d[id, ], aes(x = fpr, y = tpr), size = 4, colour = "red")
ggsave("chapters/ml/ml_imbalance/figure/threshold_tuning12.png", p1, height = 4, width = 4)

d = rocdata$data
pred = setThreshold(pred, 0.3131)
perf.new = performance(pred, measures = list(fpr, tpr))
p1 = p + geom_point(x = perf.new[1], y = perf.new[2], size = 4, colour = "red")
ggsave("chapters/ml/ml_imbalance/figure/threshold_tuning22.png", p1, height = 4, width = 4)

