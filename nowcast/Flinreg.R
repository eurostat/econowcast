# jj: step-ahead
# z: target, vector
# f: factor which comes from standardised data
# ysd: the standard deviation of target
# ymu: the mean of target
jj <- 1
zreg <- as.matrix(z[(jj+1):NROW(z),])
freg <- as.matrix(f[1:(NROW(f)-jj),])
out <- lm(zreg~freg)
b <- out$coefficients;

# Now make sure to use ysd, ymu as we used factors from standardised input 
outf <- ((f[NROW(f),]%*%b[2:NROW(b)]) + b[1])*ysd+ymu

# For one-step forecasts for time series, the residual standard deviation
# provides a good estimate of the forecast standard deviation. 
# see https://www.otexts.org/fpp/2/7
# Hyndman
#
# Not entirely correct
# but the scaling in the first step causes problems
sigmah <- sd(out$residuals*ysd+ymu)
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
