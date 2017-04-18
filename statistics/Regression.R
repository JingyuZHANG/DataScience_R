#####################
#Perform the multiple regression analysis
#####################

#Basic assumption
stud <- read.csv("D:/statistics/Statistics with R/csv-data-frames/students.csv")
fit <-lm(score~iq+hours, data = stud)
summary(fit)

##to detect the outliers, we get the standardized residuals and check whether there are values greater than 3
res <- residuals(fit)
zres <- scale(res)
View(zres)

### to check the independence of error we use the Durbin-Watson test
require(car)
durbinWatsonTest(fit)  #p value 0.114 mean we do not have correlation
# use lmterst
require(lmtest)
dwtest(fit)
####to check multicollinearity we compute the VIP(variance inflation factor)
#first we create a new data frame with the independents only
x <- data.frame(stud$iq, stud$hours)
require(usdm)
#vif =1.04 there are mot multicollinearity if vif<10
vif(x)
###to check homoskedasticity, we must plot the residuals against the fitted (predicted) test score values
require(ggplot2)
pred <- fitted(fit)
dat <- data.frame(pred,res)
View(dat)
#that is fine if chart not come to one corner
ggplot()+geom_point(data=dat, aes(x=res, y=pred))
#we check for the normality of the residuals
#p-vale = 0.59 mean residuals are normal distributed
shapiro.test(res)

####################################
#perform the multiplke regression analysis with DUMMY variables
####################################
fit <- lm(score~iq+hours+gender, data = stud)
summary(fit)

####################################
#perform hierachical regression analysis
####################################
#block 1:iq
#block 2:iq and holur
#block 3:iq, hour, and gender
fit1 <- lm(score~iq, data = stud)
fit2 <- lm(score~iq+hours, data = stud)
fit3 <- lm(score~iq+hours+gender, data = stud)
summary(fit1)
summary(fit2)
summary(fit3)
anova(fit1, fit2, fit3)

#######################################
#compute Cronbach's alpha
#######################################
require(psy)
cronbach(brd)
#or
require(fmsb)
CronbachAlpha(brd)

