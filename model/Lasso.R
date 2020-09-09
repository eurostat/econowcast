# Example Code Lasso
rm(list=ls())
install.packages("glmnet") # download and install package

library (glmnet)

#generate some artificial data
set.seed(1)
n <- 200  # Number of observations
p <- 300  # Number of predictors included in model
beta<- matrix(c(rep(1,p/2),rep(0,p/2)))
 #sparse model, some slope coefficients are zero
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- x%*%beta + rnorm(n) 
# generate the dependant variable y

fit.lasso <- glmnet(x, y, family="gaussian", alpha=1) #select Lasso penalty

nforecast=5
xnew <- matrix(rnorm(nforecast*p), nrow= nforecast, ncol=p)

predict(fit.lasso, newdata=xnew) # generate predictions based on estimated coefficients
