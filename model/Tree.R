rm(list=ls())
# Regression Tree

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

#Estimate Standard Regression tree
on the atrificial data
fit.trees<- rpart(y~x)
prp(fit.trees)

#Estimate pruned Regression tree on the atrificial data
bestcp        <- trees$cptable[which.min(trees$cptable[,"xerror"]),"CP"]
fit.prunedtree          <- prune(fit.trees,cp=bestcp)
prp(fit.prunedtree)
