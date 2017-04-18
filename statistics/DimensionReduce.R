j <- read.csv("D:/statistics/Statistics with R/csv-data-frames/juices.csv")
##################################
#how to perform the multidimensional scaling procedure
#when the data are NOT distances between objects
##################################

#we will generate two "hidden attributres" for our brands
#
j1<-t(j)
#compute the distances between brands
prox <- dist(j1)
print(prox)
#we apply the cmdscale function to the new matrix prox
#requiring the program to generate two dimensions
model <- cmdscale(prox, k=2)
print(model)
coord <- as.data.frame(model)
View(coord)
coord <- cbind(coord, c("b1","b2","b3","b4","b5","b6","b7","b8","b9","b10","b11"))
colnames(coord) <- c("attribute1","attribute2","brand")
require(ggplot2)

#################################################
#How to perform the multidimensional scaling procedure
#when the data ARE distances between objects
#################################################
dest <- read.csv("D:/statistics/Statistics with R/csv-data-frames/destinations.csv")
View(dest)

model<- cmdscale(dest)

################################################
# how to perform an exploratory factor analysis
# PCA
################################################

brd <- read.csv("D:/statistics/Statistics with R/csv-data-frames/brandsurvey.csv")
pcamodel <- princomp(brd, cor=T) # cor=T means to use correlation matrix, if set to F means to use covariance
summary(pcamodel)
#kaiser criterion : eigenvalue > 1
#the eigenvalue should be higher than one
# compute the eigenvalues
eigenv <- pcamodel$sdev^2
print(eigenv)

#Evrard criterion
#Visual inspection of the scree plot
screeplot(pcamodel, type = "line")
#based on the pca results we decide to retain three factors
#run the factor analysis with the varimax rotation
model <- factanal(brd, factors = 3, rotation = "varimax")
print(model, digits = 2, cutoff = .3, sort = TRUE)

#compute the communalities

comm <- 1- model$uniquenesses
print(comm)

#factor analysis - adequacy tests
#compute the Kaiser-Meier-Olkin measure
#get correlation matrix
corm <- cor(brd)
require(psych)
KMO(corm)

#alternatively
KMO(brd)

#compute the Bartlett's test
cortest.bartlett(corm,106)
#alternatively
cortest.bartlett(brd)

#################################################
#Simple correspondence analysis - if there is a correlation between two variables
#################################################
toyo <- read.csv("D:/statistics/Statistics with R/csv-data-frames/toyota.csv")
View(toyo)

#create a contingency table with our variables
tt <- xtabs(~model+continent, data = toyo)
print(tt)

#run the analysis and display the results
require(ca)
model <- ca(tt)
summary(model)

#plot the profiles
rco <- ca(tt)$rowcoord  # coordinates of the car models
cco <- ca(tt)$colcoord  # coordinates of the continents

rcodata <- data.frame(rco)
ccodata <- data.frame(cco)

require(ggplot2)


#################################################
#multiple correspondence analysis - if there is a correlation between  variables
#################################################
retail <- read.csv("D:/statistics/Statistics with R/csv-data-frames/retail.csv")
require(MASS)
model <- mca(retail, nf=2)
print(model)
#prepair plot
mtable<-model$cs
ccodata <- data.frame(mtable)
