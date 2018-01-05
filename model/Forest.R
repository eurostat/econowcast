rm(list=ls())
# Random Forest

# download and install packages
install.packages("ISLR") # download and install package
install.packages("randomForest") # download and install package
install.packages("rpart") # download and install package
install.packages("rpart.plot") # download and install package

library (ISLR)
library(randomForest) 
library(rpart)
library(rpart.plot)

#generate some artificial data
set.seed(1)

n <- 200  # Number of observations
p <- 300  # Number of predictors included in model
beta<- c(10/(1:p)^2)
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- x%*%beta + rnorm(n)*4

#Estimate a Random forest on the atrificial data  
RFfit<- tuneRF(x, y, mtryStart=floor(sqrt(ncol(x))),stepFactor=1.5, improve=0.05, nodesize=5, ntree=2000, doBest=TRUE)

#Find the best fir for the Random forest on the atrificial data  
min             <- RFfit$mtry
fit.rf2 <-randomForest(x, y, nodesize=5, mtry=min, ntree=2000)
