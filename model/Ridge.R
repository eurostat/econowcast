# Example Code Ridge
rm(list=ls())
install.packages("glmnet") # download and install package

library (glmnet)

#generate some artificial data
set.seed(1)

n <- 200  # Number of observations
p <- 300  # Number of predictors included in model
beta<- c(1/(1:p)^2)
#approximately sparse model, slope coefficients small but not zero
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- x%*%beta + rnorm(n)
#generate the dependant variable y

fit.ridge <- glmnet(x, y, family="gaussian", alpha=0)

predict(fit.info, newdata=x.test) # generate predictions based on estimated coefficients
