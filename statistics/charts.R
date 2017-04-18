require(ggplot2)
demo <- read.csv("D:/statistics/Statistics with R/csv-data-frames/demographics.csv")
###################################################
# histograms
###################################################
#y axis we will presnet the counts
ggplot() + geom_histogram(data = demo, aes(x=income))
#change the bins color and border
ggplot() + geom_histogram(data=demo, aes(x=income), fill = "red", color = "black")
#represent the desity on the y axis (relative frequencies)
ggplot() + geom_histogram(data = demo, aes(x=income, y = ..density..), fill = "red", color = "black")
#create multiple histograms on the same chart
ggplot() + geom_histogram(data = demo, aes(x=income, y = ..density..), fill = "red", color = "black") + 
          facet_grid(gender~marital)
#create mistograms in the same chart
ggplot() + geom_histogram(data = demo, aes(x=income, y = ..density.., fill = gender), color = "black")

#########################################
# line chart
#########################################
require(ggplot2)
require(plyr)
#create a data frame with the unique income
mydata <- count(demo, 'income')
#copute the cumulative counts and the percentage
cumul <- cumsum(mydata$freq)
cumperc <- cumul/nrow(demo)
mydata <- cbind(mydata, cumperc)
ggplot() + geom_line(data=mydata, aes(x=income,y=cumperc))
ggplot() + geom_step(data=mydata, aes(x=income,y=cumperc))

#create grouped cumulative frequencies lines
#first create two databases, by gender , using hte brackets
male <- demo[demo$gender=="Male",]
female <- demo[demo$gender=="Female",]
# form the male data frame
mydata_male <- count(male,"income")
cumulm <- cumsum(mydata_male$freq)
cumpercm <- cumulm/nrow(male)
mydata_male <- cbind(mydata_male,cumpercm)
#female
mydata_female <- count(female,"income")
cumulf <- cumsum(mydata_female$freq)
cumpercf <- cumulf/nrow(female)
mydata_female <- cbind(mydata_female,cumpercf)
#build chart
ggplot() + geom_line(data=mydata_male, aes(x=income,y=cumpercm),color="red") +
  geom_line(data = mydata_female, aes(x=income, y=cumpercf),color="blue")
# add a legent to the chart
lgd <- scale_color_manual("legend", values=c(Male="red",Female="blue"))
ggplot() + geom_line(data=mydata_male, aes(x=income,y=cumpercm,color="Male"), size=1.3) +
  geom_line(data = mydata_female, aes(x=income, y=cumpercf,color="Female"), size=1.3) +
  lgd

#########################
# column chart 
########################
require(ggplot2)
ggplot(demo, aes(x=educ, y=income, fill=educ))+
  stat_summary(fun.y=mean, geom="bar")
#if you want the same color for the bins
ggplot(demo, aes(x=educ, y=income))+
  stat_summary(fun.y=mean, geom="bar", fill="red")
# create a clustered bar chart (by the variable gender)
ggplot(demo, aes(x=educ, y=income, fill=gender))+
  stat_summary(fun.y=mean, geom="bar", position = position_dodge())

ggplot(demo, aes(x=educ, y=income, fill=gender))+
  stat_summary(fun.y=mean, geom="bar", position = position_dodge())


ggplot(demo, aes(x=educ, y=income, fill=gender))+
  stat_summary(fun.y=mean, geom="bar", position = position_stack())

################################
#MEAN plot chart
################################
# we will create a mean plot representing the average income
# for each gender category
# create the dataframe with the means of the gender groups
# Female 84
# Male   73
aggdata <- aggregate(demo$income, by=list(demo$gender),FUN=mean)
#draw the plotplot
#the x axis is defined as discrete, with convenient labels
ggplot()+geom_line(data = aggdata, aes(x=(1:2), y = aggdata$x)) + 
                     scale_x_discrete(name="Gender", labels=c("Female","Male")) +
                     scale_y_continuous(name="Income", limits = c(72,85))

ggplot()+geom_line(data = aggdata, aes(x=(1:2), y = aggdata$x), color = "red", size = 1.3) + 
  scale_x_discrete(name="Gender", labels=c("Female","Male")) +
  scale_y_continuous(name="Income", limits = c(72,85))

# build the chart wiht a polytomous factor (multiple level)
# the factor will be education level (educ)
aggdata <- aggregate(demo$income, by=list(demo$educ), FUN=mean)
ggplot()+geom_line(data = aggdata, aes(x=(1:5), y = aggdata$x)) + 
  scale_x_discrete(name="Education Level", labels=c("College degree","Did not complete High school","High School degree", "Post-undergraduate degree","Some college")) +
  scale_y_continuous(name="Income", limits = c(64,116))

#build a grouped mean plot
#the grouping variable will be the car category(carcat)
#create three data frames for the economy, standard and luxury cars
demo_ec <- demo[demo$carcat=="Economy",]
demo_st <- demo[demo$carcat=="Standard",]
demo_lu <- demo[demo$carcat=="Luxury",]
agg_ec <- aggregate(demo_ec$income, by=list(demo_ec$educ),FUN=mean)
agg_st <- aggregate(demo_st$income, by=list(demo_st$educ),FUN=mean)
agg_lu <- aggregate(demo_lu$income, by=list(demo_lu$educ),FUN=mean)

## plot the three lines on the same graph
ggplot()+
  geom_line(data=agg_ec, aes(x=(1:5), y=agg_ec$x),
            color="green")+
  geom_line(data=agg_st, aes(x=(1:5), y=agg_st$x),
            color="red")+
  geom_line(data=agg_lu, aes(x=(1:5), y=agg_lu$x),
            color="blue")+
  scale_x_discrete(name="Education Level",
                   labels=c("College degree", "Did not complete high school", 
                            "High school degree", "Post-undergraduate degree", "Some
                            college"))+
  scale_y_continuous(name="Income", limits=c(15, 220))

#Add legend

lgd <- scale_color_manual(name="Legend",
                          values=c(Economy="green", Standard="red", Luxury="blue"))
ggplot()+
  geom_line(data=agg_ec, aes(x=(1:5), y=agg_ec$x,
                             color="Economy"))+
  geom_line(data=agg_st, aes(x=(1:5), y=agg_st$x,
                             color="Standard"))+
  geom_line(data=agg_lu, aes(x=(1:5), y=agg_lu$x,
                             color="Luxury"))+
  scale_x_discrete(name="Education Level",
                   labels=c("College degree", "Did not complete high school",
                            "High school degree", "Post-undergraduate degree", "Some
college"))+
  scale_y_continuous(name="Income", limits=c(15, 220))+lgd

###########################################
# Scatterplot charts
###########################################
require(ggplot2)
hw <- read.csv("D:/statistics/Statistics with R/csv-data-frames/hw.csv")
ggplot() + geom_point(data=hw, aes(x=height, y=weight)) +
  scale_x_continuous(limits = c(150,193))
# build clustered chart by gender with color
lgd <- hw$gender
ggplot() + geom_point(data=hw, aes(x=height, y=weight, color=lgd)) +
  scale_x_continuous(limits = c(150,193))
ggplot() + geom_point(data=hw, aes(x=height, y=weight, shape=lgd, color=lgd)) +
  scale_x_continuous(limits = c(150,193))

#add a trendline to the scatterplot
#with weight as the dependent variable and height as the explainer
model <- lm(weight~height, data=hw)
print(model)
minh <- min(hw$height)
maxh <- max(hw$height)
height <- c(minh,maxh)
#predict the weight based on the height, with the model
fit <- predict(model, data.frame(height))
print(fit)
endpoints <- data.frame(height, fit)
ggplot() + geom_point(data=hw, aes(x=height, y=weight))+
  geom_line(data = endpoints,aes(x=height,y=fit),color="red", size=1)

########################
#box plot
########################

require(ggplot2)
demo <- read.csv("D:/statistics/Statistics with R/csv-data-frames/demographics.csv")

ggplot() + geom_boxplot(data= demo, aes(x=gender, y=income)) +
  scale_x_discrete(labels=c("Female", "Male"))
#set color
ggplot() + geom_boxplot(data= demo, aes(x=gender, y=income), outlier.color = "red", outlier.shape = 4) +
  scale_x_discrete(labels=c("Female", "Male"))
# add legend
lgd <- demo$gender
ggplot() + geom_boxplot(data= demo, aes(x=gender, y=income, fill=lgd), outlier.color = "red", outlier.shape = 4) +
  scale_x_discrete(labels=c("Female", "Male"))
#build a clustered boxplot
lgd <-demo$marital
ggplot() + geom_boxplot(data= demo, aes(x=gender, y=income, fill=lgd), outlier.color = "red", outlier.shape = 4) +
  scale_x_discrete(labels=c("Female", "Male"))
