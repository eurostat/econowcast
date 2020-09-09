# Example Code Elastic Net
rm(list=ls())
install.packages("glmnet") # download and install package

library (glmnet)

#generate some artificial data
set.seed(1)
n <- 200  # Number of observations
p <- 300  # Number of predictors included in model
beta1<- c(1/(1:p/2)^2)
beta<- matrix(c(beta1,rep(0,p/2)))
#combianation between sparse and approximately sparse coefficient vector
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- x%*%beta’ + rnorm(n) 
# generate the dependant variable y

fit.elnet <- glmnet(x, y, family="gaussian", alpha=0.4) #gives 40% weight to Lasso penalty

nforecast=5
xnew <- matrix(rnorm(nforecast*p), nrow= nforecast, ncol=p)

predict(fit.elnet, newdata=xnew) # generate predictions based on estimated coefficients
