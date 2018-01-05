# If necessary include lags of the regressors
# f: regressors / factors, matrix
# z: observed dependent variable, vector
# vlag: how many lags? numeric
if(vlag>0){
  f <- lagmv(f, vlag)
  f <- cbind(lagf(z, vlag)[,2:(vlag+1)], f)
}

# jj: step ahead to correctly lead/lag the relationship
#		between z, f
jj <- 1
zreg <- as.matrix(z[(jj+1):NROW(z),])
freg <- as.matrix(f[1:(NROW(f)-jj),])
# Linear regression
out <- lm(zreg~freg)
# Extract betas
b <- out$coefficients; 
# Use the last observed values to calculate forecast/nowcast
outf <- (f[NROW(f),]%*%b[2:NROW(b)])+b[1]

# For one-step forecasts for time series, the residual standard deviation
# provides a good estimate of the forecast standard deviation. 
# see https://www.otexts.org/fpp/2/7
# Hyndman
sigmah <- sd(out$residuals)
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