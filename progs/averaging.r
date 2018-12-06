# zave <- mean(z[(NROW(z)-zp+1):NROW(z),])
# boots <- matrix(NA, NROW(z), B)
# for(j in 1:B){
#   boots[,j] <- z[round(runif(NROW(z), 1, NROW(z)))]
# }
# zboot <- colMeans(boots[(NROW(boots)-zp+1):NROW(boots),])
# zout <- c(zave, quantile(zboot, probs=pintv))

zave <- mean(z[(NROW(z)-zp+1):NROW(z),])
b <- round(NROW(z)^(1/3))
boots <- matrix(NA, NROW(z), B)
for(j in 1:B){
  boots[,j] <- MBB(z, b)
}
zboot <- colMeans(boots[(NROW(boots)-zp+1):NROW(boots),])

if(YTRANSF==3){
  zlast <- YSAV[NROW(YSAV)]
  zboot <- zlast*(1+zboot)
  zave <- zlast*(1+zave)
}
if(YTRANSF==2){
  zlast <- YSAV[NROW(YSAV)]
  zboot <- zlast +zboot
  zave <- zlast + zave
}

zout <- c(zave, quantile(zboot, probs=pintv))
