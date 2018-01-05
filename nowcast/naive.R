# Input: "z" is the dependent variable, vector of data
# 		here we extract the last observed value
zf <- z[NROW(z)]

# Bootstrap for density estimation
# b: bootstrap window length
# B: number of bootstraps
b <- round(NROW(z)^(1/3))
boots <- matrix(NA, NROW(z), B)
for(j in 1:B){
  boots[,j] <- MBB(z, b)
}
# Extract the corresponding naives
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
