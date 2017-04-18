
sp <- read.csv("D:/statistics/Statistics with R/csv-data-frames/spanish.csv")
##################
#independent sample t test (two group)
##################
#We will determine whether there is a significant difference in average grade
#between the students who took the Spanish course and those who did not take it

#Basic assumptions
#the dependent variable is normally distributed in both groups
#the dependent variable does not present important outliers in any group
#the group variances are equal

#to check the assumption of homogeneity of variances, we will use the leveneTest function in the car package
require(car)
#F value is 0.89 and P-value is 0.35 mean we accept null hypothesis that two group are equal
leveneTest(sp$score,sp$course)
#run the t-test
t.test(sp$score~sp$course, var.equal=T)

#if the group variances are NOT equal we will execute the Welch test
t.test(sp$score~sp$course, var.equal=F)

####################################
#paired sample t test
####################################
mat <- read.csv("D:/statistics/Statistics with R/csv-data-frames/math.csv")
View(mat)
#basic assumption:
#the differences between the dependent variables are normally distributed
#the differences between the dependent variables do not present important outliers
############
t.test(mat$grade2,mat$grade1, paired = T)


vit <- read.csv("D:/statistics/Statistics with R/csv-data-frames/vitamin1.csv")
############################
#How to perform the one-way analysis of variance (intermediate level)
############################
#We will check whether there is a difference between the three groups(placebo, low dose, high dose) with respect to the average effort resistance
#Basic assumptions
#the dependent variable is normally distributed in all groups
#the dependent variable does not present important outliers in any group
#the group variances are equal with leveneTest
require(car)
#F value is 0.89 and P-value is 0.47 mean we accept null hypothesis that two group are equal
leveneTest(vit$effort,vit$dose)
aov1 = aov(effort~dose, data=vit)
summary(aov1)
#if the group variances are NOT equal, we run the welch test
oneway.test(effort~dose, data=vit, var.equal = F)
#how to perform the simple(post-hoc) comparisons
###Tukey HSD for equal variances
TukeyHSD(aov1)
#Bonferroni also for equal variances
pairwise.t.test(vit$effort,vit$dose, p.adjust.method = "bonferroni")

####################################
#two way analysis of variance
####################################
vit <- read.csv("D:/statistics/Statistics with R/csv-data-frames/vitamin2.csv")
#to check the equality of variances, we must create a separate variable to define the six groups with plyr package
aov1 = aov(effort~dose+gender+dose*gender, data = vit)
summary(aov1)

###########################
#compute the simple main effecrts  
#the simple main effect of a factor is the effect of that factor at every level of the other factor
###########################
vitp = vit[vit$dose=="placebo",]
aov1 = aov(effor~gender, data=vitp)
summary(aov1)
#perform the Tukey HSD (simple comparison)
TukeyHSD(aov1)
###dose = low
vitld = vit[vit$dose=="low dose",]
aov2 = aov(effort~gender, data=vitld)
summary(aov2)
TukeyHSD(aov2)
###dose = high
vithd = vit[vit$dose=="high dose",]
aov3 = aov(effort~gender, data=vitld)
summary(aov3)
TukeyHSD(aov3)

####################################
#how to perform the three-way of analysis of variance
####################################
vitm <- read.csv("D:/statistics/Statistics with R/csv-data-frames/vitamin3.csv")

#########################################
#simple second order interaction effects
#########################################
#3 second order interaction effect
#dose * gender for each type of employee
#dose * type
#gender * type
vitm <- vit[vit$gender=="male",]
aov1 <- aov(effort~dose*type, data = vitm)
summary(aov1)

#############################################
# Mann-Whitney test -- check whether there is a difference between the two groups of students
#                      with respect to the average test score
#############################################
require(psych)
sp <- read.csv("D:/statistics/Statistics with R/csv-data-frames/spanish.csv")
wilcox.test(sp$score~sp$course)
wilcox.test(sp$score~sp$course, correct=F)
describeBy(sp$score,sp$course)
#Wilcoxon test
mat <- read.csv("D:/statistics/Statistics with R/csv-data-frames/math.csv")
wilcox.test(mat$grade1,mat$grade2, paired = T)


##############################
#Kruskall-wall  -- we are going to check whether there is a singnificant difference
#                  between the three groups with respect to the average effort resistance
##############################
vit <- read.csv("D:/statistics/Statistics with R/csv-data-frames/vitamin1.csv")
kruskal.test(vit$effort~vit$dose)
require(psych)
describeBy(vit$effort,vit$dose)
