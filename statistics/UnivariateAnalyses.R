###################
#one sample t-test
###################
demo <- read.csv("D:/statistics/Statistics with R/csv-data-frames/demographics.csv",stringsAsFactors=FALSE)
#basic assumption on sample t-test:
#the variable of study is normally distributed
#the variable does not present important outliers
##################################################
#we will check whether the average income is singnificantly different from 70
t.test(demo$income, alternative = "two.sided", mu=70)

##############################
#binomial test
##############################
#we will check whether the male/female proportion in the population is 50/50
# crete a counts table for the variable gender
mytable = table(demo$gender)
View(mytable)
#run the binomial test
binom.test(mytable, p=0.50, alternative = "two.sided", conf.level = 0.95)
#to check whether the proportion of male subject is 60%
binom.test(mytable, p=0.40, alternative = "two.sided", conf.level = 0.95)

###########################
#chi square test for goodness-of-fit
###########################
mytable <- table(demo$educ)
#run the chi square test with equal theoretical probabilities
n <- length(mytable)
thprop <- 1/n
#x-squared =70, p-value = 0.0000000000000001 mean the properbility is not equal distribution
chisq.test(mytable, p=rep(thprop, n))
#get the expected values, the residual values and the standardized residuals
chisq.test(mytable, p=rep(thprop, n))$expected
chisq.test(mytable, p=rep(thprop, n))$residuals
chisq.test(mytable, p=rep(thprop, n))$stdres

#run chi square test with unequal theoretical probabilities
chisq.test(mytable, p=c(0.30,0.30,0.20,0.10,0.10))
chisq.test(mytable, p=c(0.30,0.30,0.20,0.10,0.10))$expected
#if some expected counts are lower than 5, we can ask the program to simulate the p value
chisq.test(mytable, p=rep(thprop,n), simulate.p.value = TRUE)

bf <- read.csv("D:/statistics/Statistics with R/csv-data-frames/breakfast.csv",stringsAsFactors=FALSE)
####chi square test for associate for the variables agecat and bfast
CrossTable(bf$agecat,bf$bfast, expected = T, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
# if some expected values are lower than5, we can compute the Fisher's exact test
#p-value=0.0000005 mean there are not strong association between age and breakfast
require(gmodels)
fisher.test(bf$agecat, bf$bfast, simulate.p.value = T)
##how to compute the Cramer's V --compute how strong corrlation 0.3-0.7
require(lsr)
cramersV(bf$agecat, bf$bfast)

demo <- read.csv("D:/statistics/Statistics with R/csv-data-frames/hw.csv",stringsAsFactors=FALSE)

#######################
#how to perform th Pearson correlaton
#######################
#Basic assumptions:
#the variables are normally distributed
#there are no significant outliers
#the relationship between the variables is approximately linear

#run the correlation test
#cor 0.81
cor.test(hw$height, hw$weight, method = "pearson", alternative = "two.sided", conf.level = 0.95)
cor.test(hw$height, hw$weigh)
#Spearman correlation
cor.test(hw$height, hw$weight, method = "spearman", conf.level = 0.95, exact = FALSE) #exact = FALSE in order to force the program
#package pspearman
require(pspearman)
spearman.test(hw$height,hw$weight, approximation = "AS89") #"AS89" is an approximation algorithm used to compute the p-value
#Kendall corelation
cor.test(hw$height, hw$weight, method = "kendall", conf.level = 0.95, exact = FALSE)

#####################################
# How to perform partial correlation
#####################################
require(ppcor)
ice <- read.csv("D:/statistics/Statistics with R/csv-data-frames/icecream.csv",stringsAsFactors=FALSE)
cor.test(ice$attacks, ice$icecream)
#p.value = 0.4 mean there is not corrlation between attacks and icecream
pcor.test(ice$attacks, ice$icecream, ice$temp, method = "pearson")
