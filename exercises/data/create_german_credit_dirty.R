library(data.table)
data = as.data.table(read.csv("german_credit.csv"))
y = data[, "credit_risk"]
x = data[, - "credit_risk"]

set.seed(2409)
# outlier for amount
x[sample(.N, size = 1L), amount := 80000L]
# NA and outlier for age
x[sample(.N, size = ceiling(0.096 * .N), replace = FALSE), age := NA_integer_]
x[sample(.N, size = ceiling(0.041 * .N), replace = FALSE), age := 0L]
# NA for employment_duration
x[sample(.N, size = ceiling(0.087 * .N), replace = FALSE), employment_duration := NA_character_]
# NA for number_credits
x[sample(.N, size = ceiling(0.049 * .N), replace = FALSE), number_credits := NA_character_]

german_credit_dirty = cbind(y, x)

write.table(german_credit_dirty, file = "german_credit_dirty.csv", sep = ",", dec = ".")

