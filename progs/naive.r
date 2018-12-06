zf <- z[NROW(z)]
b <- round(NROW(z)^(1/3))
boots <- matrix(NA, NROW(z), B)
for(j in 1:B){
  boots[,j] <- MBB(z, b)
}
zboot <- boots[NROW(boots),]

if(YTRANSF==3){
  zlast <- YSAV[NROW(YSAV)]
  zboot <- zlast*(1+zboot)
  zf <- zlast*(1+zf)
}
if(YTRANSF==2){
  zlast <- YSAV[NROW(YSAV)]
  zboot <- zlast +zboot
  zf <- zlast + zf
}

zout <- c(zf, quantile(zboot, probs=pintv))
