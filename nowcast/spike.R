# jj: step-ahead
# z: target, vector
# f: factor which comes from standardised data
# ysd: the standard deviation of target
# ymu: the mean of target
lag <- 1
if(vlag>0){
  # XX <- lagmv(XX, vlag)   # no because of small T dimension
  XX <- cbind(lagf(YY, vlag)[,2:(vlag+1)], XX)
  
  XX <- as.matrix(XX[(vlag+1):NROW(XX),])
  YY <- as.matrix(YY[(vlag+1):NROW(YY),])
}
# Standardise
xxin <- xstd(XX)
ymu <- mean(YY)
ysd <- sd(YY)
yyin <- (YY-ymu)/ysd

z <- yyin
f <-xxin

jj <- 1
zreg <- as.matrix(z[(jj+1):NROW(z),])
freg <- as.matrix(f[1:(NROW(f)-jj),])
# Spike and Slab
# niter: The number of MCMC iterations to run
# ping: output printing parameter
out <- lm.spike(zreg ~ freg, niter=niters, ping=B)
# keep the last B rounds
b <- out$beta
b <- b[(NROW(b)-B+1):NROW(b),]
bf <- colMeans(b)
outf <- ((f[NROW(f),]%*%bf[2:NROW(bf)]) + bf[1])*ysd+ymu
#Calculate percentiles
sigmah <- sd((zreg-freg%*%bf[2:NROW(bf)]-bf[1])*ysd+ymu)
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