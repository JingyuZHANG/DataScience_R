########################################
#perform the binomial logistic regression
#we will predict the chance that a subject uses movbile Internet based on the other three variables
########################################
mobi <- read.csv("D:/statistics/Statistics with R/csv-data-frames/mobilenet.csv")
#the dependent variable is dichotomous (binary) at home(0) at office(1)
#Basic assumptions
#the dependent variables do not present outliers
#there is no important multicollinearity
model <- glm(mobile~income+hours+where, data = mobi,family = binomial())
summary(model)

############compute the antilogarithms of the coefficients
############these antilogs actually represent the chance  that a subject use mobile Internet

expb <- exp(coef(model))
print(expb)
###compute the confidence interval of the antilogarithms
intexp <- exp(confint(model))
print(intexp)

##########################
#the binomial logistic regression - goodness of fit indicators
##########################
model <- glm(mobile~income+hours+where, data = mobi,family = binomial())
#compute the Hosmer-Lemeshow statistic - shown how bad the depedent variable fit in independent variable
require(ResourceSelection)
hoslem.test(mobi$mobile, fitted(model))

#compute the Nagelkerke pseudo R square
require(fmsb)
NagelkerkeR2(model)

#compute all the pseudo R square indicatiors
require(BaylorEdPsych)
PseudoR2(model)

###################################
#multinormial logistic regression
###################################
news <- read.csv("D:/statistics/Statistics with R/csv-data-frames/newspapers.csv")
#dependent variable: preferred newspaper, with 3 categories
#Daily News, National Politics, Free Tribune
#explainers: age and political opinion
#political is a categorical variable with three categories: left-wing, right-wing, center

#before running the regression, we must set the reference category(baseline)
#for the categorical varibable in the model
#set the baseline
news$newspaper <- relevel(news$newspaper, ref = "Free Tribune")
news$political <- relevel(news$political, ref = "Center")

require(nnet)
model <- multinom(newspaper~age+political, data = news)
summ <- summary(model)
print(summ)
# compute the z scores
z <- summ$coefficients/summ$standard.errors
#generate the p Values of the z score (two-tailed)
pv <- pnorm(abs(z), lower.tail = F) *2
print(pv)

#compute the antilogarithms of the coefficients
expb <- exp(coef(model))
print(expb)
#compute the confidence intervals for the coefficients
ci <- confint(model, level = 0.95)
print(ci)
#compute the confidence intervals for the antilogarithms
expci <- exp(ci)
print(expci)

#compute the predicted 
pred <- fitted(model)
View(pred)

#################################
#multinomial logistic regeression - goodness of fit measures
#################################
require(nnet)
# set reference categories
news$newspaper <- relevel(news$newspaper, ref = "Free Tribune")
news$political <- relevel(news$political, ref = "Center")

### create the null model (without explainers)
model0 <-multinom(newspaper~1, data = news)
model <- multinom(newspaper~age+political, data = news)

LL1 <-logLik(model)
LL0 <- logLik(model0)

#McFadden pseudo R square =.0.43
mcfadden <- 1-(LL1/LL0)
print(mcfadden)

#Cox-Snell pseudo R square
n<-nrow(news)
coxsnell <- 1- exp((2/n) * (LL1-LL0))

# get deviance
deviance(model)


###########################################
#Ordinal logistic Regression
###########################################
satis <- read.csv("D:/statistics/Statistics with R/csv-data-frames/satisfaction.csv")
View(satis)
##N.B. the ordinal variables must be coded numerically the nominal variables can be string
require(MASS)
satis$imprice <- relevel(factor(satis$imprice),ref = "3")
satis$type <- relevel(satis$type, ref = "Business traveler")
model <- polr(factor(satisfaction)~type+age+imprice, data = satis, method = "logistic")
summary(model)

#####compute the p values for the coefficients
cft <- coef(summary(model))
print(cft)

pv <- pnorm(abs(cft[,"t value"]), lower.tail = F) *2
print(pv)
cft <- cbind(cft, "p value" =pv)
print(cft)

#Ordianl logistic regression - interpreting the antilogarithms (odds)
#let's run the model again
require(MASS)
satis$imprice <- relevel(factor(satis$imprice),ref = "3")
satis$type <- relevel(satis$type, ref = "Business traveler")
model <- polr(factor(satisfaction)~type+age+imprice, data = satis, method = "logistic")

#compute the odds (antilogarithms of the coefficients)
#typePleasure traveler                   age              imprice1              imprice2 
#              1.943764 (mean 94.37% important) 1.273652  1.037649              3.194479 
odds <- exp(coef(model))
print(odds)

#get the confidence interval for the odds
ci <- exp(confint(model))
print(ci)

########################################################
#Ordianl logistic regression - goodness-of-fit
########################################################
require(MASS)
satis$imprice <- relevel(factor(satis$imprice),ref = "3")
satis$type <- relevel(satis$type, ref = "Business traveler")
model <- polr(factor(satisfaction)~type+age+imprice, data = satis, method = "logistic")
#compute the goodness-of-fit indicators manually
#first we fit the null model (without independent variables)
model0 <- polr(factor(satisfaction)~1, data = satis, method = "logistic")
# now we compute the log-likelihood of both null and proposed model
LL0 <- logLik(model0)
LL11 <- logLik(model)
#compute the pseudo R squares
#McFadden pseudo R square
mcfadden <- 1-(LL1/LL0)
#cCox-Snell pseudo R square
n<-nrow(satis)
coxsnell <- 1-exp((2/n)*(LL0-LL1))
print(coxsnell)

nagel <- (1-exp((2/n)*(LL0-LL1)))/(1-exp(LL0)^(2/n))
print(nagel)

#compute the deviance
deviance(model)
#get the deviance table of the model
require(car)
Anova(model)

##################################################
#checking the assumption of proportional odds
##################################################
require(ordinal)

#we will use the clm function in the package ordinal
model <- clm(factor(satisfaction)~type+age+imprice, data = satis)
nominal_test(model) #p-value >0.5
