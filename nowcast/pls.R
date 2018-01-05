# Lag the matrix of Regressors if necessary
# vlag: lag order
# XX : matrix, panel of regressors
# YY : vector, observed target
vlag <- 1
if(vlag>0){
  XX <- cbind(lagf(YY, vlag)[,2:(vlag+1)], XX)
  
  XX <- as.matrix(XX[(vlag+1):NROW(XX),])
  YY <- as.matrix(YY[(vlag+1):NROW(YY),])
}

# Standardise
xxin <- xstd(XX)
ymu <- mean(YY)
ysd <- sd(YY)
yyin <- (YY-ymu)/ysd

# Extract factors
pp <- plsr(yyin~xxin, ncomp=qncomp, scale=FALSE)
f <- as.matrix(pp$scores)
z <- as.matrix(yyin)

# Use Factor linear regression
source("./Flinreg.R")