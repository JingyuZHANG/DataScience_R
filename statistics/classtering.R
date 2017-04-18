car <- read.csv("D:/statistics/Statistics with R/csv-data-frames/cars.csv")
#Hierachical cluster analysis
car2 <- cbind(car$price, car$engine, car$power, car$fuelcons, car$speed)
colnames(car2) <- c("price","engine","power","fuelcons","speed")
rownames(car2) <- car$carmodel
View(car2)

#compute the distance matrix
dm <- dist(car2, method = "euclidean")
#create the clustering model
model <- hclust(dm, method = "ward.D")
plot(model, labels = rownames(car2))

#get cluster nembership
cutree(model, k = 2:4)
#visualize clusters on the dendrogram
rect.hclust(model, k=5, border = "red")

#####################################################################
#k-means
#####################################################################
ctr <- read.csv("D:/statistics/Statistics with R/csv-data-frames/countries.csv")
ctr <- na.omit(ctr)

#create a matrix with all the clustering variables
ctr2 <- cbind(ctr$urban, ctr$flexp, ctr$mlexp, ctr$literacy, ctr$infmort, ctr$gdp, ctr$density)

ctr2 <- scale(ctr2)
#name the rows and columns
rownames(ctr2)<- ctr$country
colnames(ctr2) <- c("urban", "flexp", "mlexp", "literacy", "infmort", "gdp", "density")

model<- kmeans(ctr2, 3)
print(model)

#get some relevant information
model$cluster
model$size
#mean of each clusters
model$centers
#sum of squares
model$totss
#within-cluster sum of squares
model$withinss
model$betweenss
model$tot.withinss
#plot the clusters
centers <- data.frame(model$centers)
ctr3 <- data.frame(ctr2)
require(ggplot2)


#####################################################
#how to perform a simple discriminant analysis
#####################################################
# we will determine whether the employee gender ca nbe predicted based on othere variables

