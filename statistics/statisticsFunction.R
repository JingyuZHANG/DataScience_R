demo <- read.csv("D:/statistics/Statistics with R/csv-data-frames/demographics.csv")

mean(demo$income)
sd(demo$income)
var(demo$income)
min(demo$income)
max(demo$income)
range(demo$income)
median(demo$income)
quantile(demo$income)

demo2 <- cbind(demo$age, demo$income, demo$carpr)
colnames(demo2) <- c("age", "income", "price")
# generate the statistics table
require(psych)
describe(demo2)
describe(demo2,na.rm = TRUE, trim = 0.1, check = TRUE)

require(pastecs)
options(scipen = 100)
options(digits = 2)
# get basic only
stat.desc(demo2)

stat.desc(demo2,basic = FALSE)

stat.desc(demo2, desc = FALSE)

# Skeness and kurtosis
require(e1071)
skew(demo$income)
skewness(demo$income)
kurtosis(demo$income)  # 3 is standard
# >3 means sharp than normal distribution
quantile(demo$income, probs = c(0.17,0.55,0.97))

#mode
require(modeest)
mlv(demo$income, method = "mfv")  ### "most frequent value"
######################
# groups or subsets
######################
require(doBy)
# income group by gender
func <- function(x){
  descStat(x,na.rm = TRUE)
}
summaryBy(income~gender, data = demo, FUN = func)
summaryBy(income+age~gender, data = demo, FUN = func)

# use package pych can not use like income+age~gender
require(psych)
describeBy(demo$income,demo$gender)

#use package stats
#compute mean age group by marital
aggregate(demo$age, by = list(demo$marital), FUN = mean)
#standard deviation
aggregate(demo$age, by = list(demo$marital), FUN = sd)
aggregate(demo$age, by = list(demo$marital), FUN = median)
aggregate(demo$age, by = list(demo$marital), FUN = var)
aggregate(demo$age, by = list(demo$marital, demo$gender), FUN = median)
