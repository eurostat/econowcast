# jj: step-ahead
# z: target, vector
# f: factor which comes from standardised data
# ysd: the standard deviation of target
# ymu: the mean of target
vlag <- 1
if(vlag>0){
  XX <- cbind(lagf(YY, vlag)[,2:(vlag+1)], XX)
  
  XX <- as.matrix(XX[(vlag+1):NROW(XX),])
  YY <- as.matrix(YY[(vlag+1):NROW(YY),])
}
z <- YY
f <- XX

# jj: step ahead, then correctly lead/lag the variables
jj <- 1
zreg <- as.matrix(z[(jj+1):NROW(z),])
freg <- as.matrix(f[1:(NROW(f)-jj),])

# Calculate beta which comes from sparse
# freg: regressors,  matrix
# zreg: target, vector
# type.measure: "mse" for the calculation of lambda
# alpha: 1 for Lasso, 0.5 for LAR
fit.lasso.cv <- cv.glmnet(freg, zreg, type.measure="mse", alpha=salpha,
                          family="gaussian", standardize=TRUE)
s <- fit.lasso.cv$lambda.min
b <- as.numeric(coef(fit.lasso.cv, s))
outf <- ((f[NROW(f),]%*%b[2:NROW(b)]) + b[1])

# Calculate percentiles
sigmah <- sd(zreg-freg%*%b[2:NROW(b)]-b[1])
spseq <- qnorm(seq(0.51, 0.99, 0.01))*sigmah
zout <- c(outf, outf-rev(spseq)[1], outf-rev(spseq), outf, outf+spseq, outf+spseq[NROW(spseq)])

if(YTRANSF==3){
  zlast <- YSAV[NROW(YSAV)]
  zout <- zlast*(1+zout)
}
if(YTRANSF==2){
  zlast <- YSAV[NROW(YSAV)]
  zout <- zlast + zout
}