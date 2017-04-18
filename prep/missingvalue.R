x<-1:10
y <- c(11,12,18,14,17, NA, NA, 19, NA, 27)
z <- c(19,11,2,14,20,4,9,10,18,1)
w <- c(1,4,7,10,3,5,7,6,6,9)
data<-data.frame(x,y,z,w)

cor(data)

# finding the most correlated variable
cor(data, use = "complete.obs")

#symnum()
symnum(cor(data, use = "complete.obs"))

Ind <- function(t){
  x <- dim(length(t))
  x[which(!is.na(t))] = 1
  x[which(is.na(t))] = 0
  return(x)
}
data$I <- Ind(data$y)
lm(y ~ x, data=data)
summary(lm(y ~ x, data=data))
# y =  9.743 + 1.590*x from lm()
for(i in 1:nrow(data))
{
  if(data$I[i] == 0)
  {
    data$y[i] = 9.743 + 1.590*data$x[i]
  }
}


library("vim")
