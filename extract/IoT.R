rm(list=ls(all=TRUE))

library(apcluster)

source("./mc-functions.R")
setwd("../data/iot")

N <- 1000 # nobs in each cluster

# specify cluster means and sds
pmean <- seq(0, 20, 6)
psd <- (1:NROW(pmean))
psd <- psd/median(psd)

x <- matrix(NA, 0, 2)
for(i in 1:NROW(pmean))
{
  x <- rbind(x, matrix(rnorm(N*2, mean=pmean[i],
                             sd=psd[i]), ncol=2))
}

colnames(x) <- c("x", "y")
cl <- kmeans(x, NROW(pmean))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex = 2)

ap <- apcluster(negDistMat(r=2), x)
plot(ap, x)

ks <- k.elbow2(x, 50)


