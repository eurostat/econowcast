vlag <- 1
if(vlag>0){
  # XX <- lagmv(XX, vlag)   # no because of small T dimension
  XX <- cbind(lagf(YY, vlag)[,2:(vlag+1)], XX)
  
  XX <- as.matrix(XX[(vlag+1):NROW(XX),])
  YY <- as.matrix(YY[(vlag+1):NROW(YY),])
}
z <- YY
f <- XX

jj <- 1
zreg <- as.matrix(z[(jj+1):NROW(z),])
freg <- as.matrix(f[1:(NROW(f)-jj),])

fit.lasso.cv <- cv.glmnet(freg, zreg, type.measure="mse", alpha=salpha,
                          family="gaussian", standardize=TRUE)
s <- fit.lasso.cv$lambda.min
b <- as.numeric(coef(fit.lasso.cv, s))
outf <- ((f[NROW(f),]%*%b[2:NROW(b)]) + b[1])

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