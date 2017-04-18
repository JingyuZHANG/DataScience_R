demo <- read.csv("D:/statistics/Statistics with R/csv-data-frames/demographics.csv")
#frequuence table

#count only
mytable <- table(demo$educ, exclude = NULL) ## the missing value will be excluded
print(mytable)
# cumulative counts 
cumul <- cumsum(mytable)
# compute the relative frequencies (the percentage)
relative <- prop.table(mytable)
print(relative)

#compute the cumulative relative frequencies
n = nrow(demo) # count of rows
cumulfreq <- cumul/n
print(cumulfreq)

# create the final table with cbind function
mytable2 <- cbind(Freq=mytable,Cumul=cumul, Relative=relative, CumFreq=cumulfreq)
print(mytable2)

#####################################
# plyr package
#####################################
require(plyr)
mytable <- count(demo,'educ')
print(mytable)
#compute the percentages (relative frequencies)
perc <- mytable$freq/nrow(demo)
print(perc)
#compute the cumulative percnetages
cumul <- cumsum(mytable$freq)
cumperc <-cumul/nrow(demo)

mytable <- cbind(mytable, cumul, perc, cumperc)
print(mytable)

##########################
# create cross-tables with variables gender and carcat (car category)
##########################
ct <- xtabs(~gender+carcat, data = demo)
ftable(ct)

require(gmodels)
#expected = TRUE shows the row proportions
#prop.r shows the row proportions
#prop.c shows the column proportions
#prop.chisq shows the chi square contributions
#chisq computes the chi square test for association
#fisher computes the Fisher exact test
#mcnemar compute the MCNemar test (for 2x2 tables only)
CrossTable(demo$gender, demo$carcat, digits = 3, expected = TRUE, prop.r = TRUE, prop.c = TRUE,
           prop.t = TRUE, prop.chisq = TRUE, chisq = FALSE, fisher = FALSE, mcnemar = FALSE)
