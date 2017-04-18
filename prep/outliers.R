data <- c(sample(x = 1:20, size = 40, replace = TRUE), 65,80)
data
summary(data)
boxplot(data)
data1 <- data
length(data1)
# discarding the outliers from the data set
bench <- 18.00 + 1.5 * IQR(data)
bench
data1[data1>35]
data1 <- data1[data1 < bench]
summary(data1)
boxplot(data1)


# winsorizzing
data[data>bench]
data[data > bench]<-bench
data


# variable transformation

log_data <- log(data)

############################################
# outlier analysis                         
# 1. incorrect entry
# 2. mis-reporting
# 3. sampling error
# 4. Exceptional but true value
############################################


