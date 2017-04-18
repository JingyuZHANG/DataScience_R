demo <- read.csv("D:/statistics/Statistics with R/csv-data-frames/demographics.csv",stringsAsFactors=FALSE)

#how to perform the Shapiro-Wilk mormality test
#will will check whether the variable income is normally distributed
# W=0.5, p-value<0.000000000000002 -mean the data is not normally distributed
shapiro.test(demo$income)
#how to check for mormality with the help of histogram
require(ggplot2)
#comput the mean and standard deviation of the income
m <- mean(demo$income)
std <- sd(demo$income)
ggplot() + geom_histogram(data=demo, aes(x=income, y = ..density..), fill="red") +
  stat_function(fun=dnorm, args = list(mean = m, sd = std), aes(x=demo$income))
############################
#Detect outliers
############################
#How to detect the outliers in a data series
#with the help of the standardized values
#compute the standardized values of the variable
zinco <- scale(demo$income, scale = TRUE)
sort(zinco, decreasing = TRUE)
zinco[zinco>3] # >3 is the out liers