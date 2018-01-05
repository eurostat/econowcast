# Input: "z" is the dependent variable, vector of data
#			zp: window length, e.g. zp=4, then we have the 4-period MA.
zave <- mean(z[(NROW(z)-zp+1):NROW(z),])

# Bootstrap for density estimation
# b: bootstrap window length
# B: number of bootstraps
b <- round(NROW(z)^(1/3))
boots <- matrix(NA, NROW(z), B)
for(j in 1:B){
  boots[,j] <- MBB(z, b) # MBB: Moving Block Bootstrap
}
# Calculate the mean value for the same zp
zboot <- colMeans(boots[(NROW(boots)-zp+1):NROW(boots),])

# YTRANSF: if 3, we translate from growth to levels
#		   if 2, we translate from 1-st diff to levels
#
#	zlast: is the last observed value for the dependent variable
#	zboot: are the bootstrap estimate for densities
# 	zave: (or different name) is the final estimate in levels.
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
