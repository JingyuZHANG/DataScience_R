demo <- read.csv("D:/statistics/Statistics with R/csv-data-frames/demographics.csv",stringsAsFactors=FALSE)
demo2 <- demo[demo$gender == "Female",]
View(demo2)
demo2 <- demo[demo$income>100, c(1,3,7)]
## drop variables 6,7 
demo2 <- demo[demo$income>100, -c(6,8)]
demo2 <- demo[demo$gender =="Female" & demo$income>100,]
# subset
demo2 <- subset(demo, gender == "Female" & income > 100)
demo2 <- subset(demo, gender == "Female" || income > 100, select = c(1:3,7))

require(dplyr)
demo2 <- filter(demo, gender == "Female", income > 100)
# dplyr do -c(1,3,7)
demo2 <- select(demo, age, marital, income)

#############################
# recode variable or revalue with plyr
#############################

demo$gender2[demo$gender == "Male"] = "1"
demo$gender2[demo$gender == "Female"] = "2"
demo
#plyr with revale new column
#IMPORTANT: if the variable to recode is not a factor
#           we must convert it into a factor before recording
demo$gender = factor(demo$gender)
require(plyr)
demo$gender3 = revalue(demo$gender, c("Male" = "1", "Female" = "2"))

# recode a continous variable into a factor
demo$incat[demo$income<200] = "Low income"
demo$incat[demo$income>=200] = "High income"
#Create 3 groups by income
#<150, 150-300,>300
demo$incat2 = cut(demo$income, breaks = c(-Inf, 150, 300, Inf), 
                  labels = c("Low income", "Medium income", "High income"), right = FALSE)

########################
#sort dataframe
########################
demo2 <- demo[order(demo$income),]
# descending
demo2 <- demo[order(-demo$income),]

demo2 <- demo[order(demo$income, -demo$age),]

#compute a new variable
demo2$test <- demo$age + demo$income
