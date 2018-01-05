
# Example Code Slab and Spike
rm(list=ls())
install.packages("BoomSpikeSlab") # download and install package

library (BoomSpikeSlab)

#generate some artificial data
set.seed(1)
n = 200 #sample size
p = 300 # number of variables
nonzerob = 3 # nubmer of variables with non-zero coefficients
niter <- 1000 # nubmer of MCMC draws
sigma <- .8
x <- cbind(1, matrix(rnorm(n * (p-1)), nrow=n))
beta <- c(rep(2,ngood),rep(0, p-nonzerob ))

y <- rnorm(n, x %*% beta, sigma)
x <- x[,-1]

#Estimate spike and Slab regression
model <- lm.spike(y ~ x, niter=niter)

#Plots of coefficients
plot.ts(model$beta)
hist(model$sigma) ## should be near 8
plot(model) 
summary(model)

#Plot residuals
plot(model, "residuals")

Xnew = cbind( matrix(rnorm(n * (p-1)), nrow=n))

#if out-of-sample forecasts are required
yhat.slab.new = predict.lm.spike(model, newdata=Xnew) #out-of-sample prediction