rm(list=ls(all=TRUE))
setwd("../data/subsample/")
library("moments")
library(rCUR)

# ***************** some functions 
# ***************** from clusterv
# Prediction of the minimum distortion of random projection for a given subspace dimension according to JL lemma
# Input:
# n : cardinality of the data
# dim : dimension of the projected subspace
# Output:
# minimum distortion
JL.predict.distortion <- function(n, dim=10) {
  epsilon <- sqrt(4 * log(n) / dim);
  epsilon
}

# Prediction of the dimension of random projection we need to obtain a given distortion according to JL lemma
# Input:
# n : cardinality of the data
# epsilon : distortion (0 < epsilon <= 0.5)
# Output:
# minimum dimension
JL.predict.dim <- function(n, epsilon=0.5) {
  d <- 4 * (log(n) / epsilon^2);
  ceiling(d)
}

# Generation of the vector containing the indices randomly selected.
# It is used by the function random.subspace
# Input:
# d : subspace dimension
# d.original : dimension of the space from which components are randomly selected 
# Output:
# vector of the selected features: it contain the indices of the components randomly selected
random.component.selection <- function(d=2, d.original=10)     {
  selected.features <- numeric(d);
  n.feat <- d.original+1;
  feat <- floor(runif(1,1,n.feat));
  selected.features[1] <- feat;
  for (i in 2:d) {
    present <- TRUE;
    while(present)  {
      feat <- floor(runif(1,1,n.feat));
      for (j in 1:(i-1)) {
        if (selected.features[j] == feat)
          break;
      }
      if ((j==i-1) && (selected.features[j] != feat)) {
        present<-FALSE;
        selected.features[i] <- feat;    
      }    
    }
  } 
  selected.features
} 

# *************** end.of.functions

t <- 10000
M <- sin((1:t)/(log(t)^3))
# M <- M + (1:t)
M <- as.matrix(M)

ds <- JL.predict.dim(NROW(M), 0.1)
d1 <- random.component.selection(d=ds, d.original=NROW(M))
d2 <- random.component.selection(d=100, d.original=NROW(M))

d1 <- sort(d1)
d2 <- sort(d2)

K1 <- M[d1,]
K2 <- M[d2,]

plot(M, type="l", main="Original Data")
plot(K1, type="l", main="Subsample 1, 3685 Obs.")
plot(K2, type="l", main="Subsample 2, 100 Obs.")

densM <- density(M)
densK1 <- density(K1)
densK2 <- density(K2)

plot(densM$x, densM$y, type="l", lwd=2,
     xlab=paste("N = ", t, ", Bandwidth = ",
                round(densM$bw, 5), sep=""), ylab="Density",
     xlim=c(min(densM$x, densK1$x, densK2$x),
            max(densM$x, densK1$x, densK2$x)))
lines(densK1$x, densK1$y, col="red")
lines(densK2$x, densK2$y, col="blue")


out.d <- cbind(rbind(mean(M), mean(K1), mean(K2)),
rbind(sd(M), sd(K1), sd(K2)),
rbind(min(M), min(K1), min(K2)),
rbind(max(M), max(K1), max(K2)),
rbind(median(M), median(K1), median(K2)),
rbind(skewness(M), skewness(K1), skewness(K2)),
rbind(kurtosis(M), kurtosis(K1), kurtosis(K2)))

write.csv(t(out.d), paste("out/N", t, "t(out.d).csv", sep=""))

# Now, what if we have columns?
# nrow <- events
# ncol <- time periods
N <- 10000
c <- 500
D <- matrix(NA, N, c)
Dp <- D
for(i in 1:c)
{
  if(i==1){
    D[,i] <- abs(rnorm(N, mean=runif(1,0,0.2),
                       sd=runif(1,0,1)))
  }else{
    D[,i] <- abs(rnorm(N, mean=(mean(D[,i-1])+rnorm(1)),
                       sd=(sd(D[,i-1])+runif(1,0,1))))
  }
  Dp[,i] <- round(runif(N, 0, 1))
}
D <- D/1000
# First create the series using average
M <- apply(D, 2, sum)
M2 <- apply(D*Dp, 2, sum)


# then subsample time and NOT events
d1 <- random.component.selection(d=100, d.original=NCOL(D))
d2 <- random.component.selection(d=250, d.original=NCOL(D))
d1 <- sort(d1)
d2 <- sort(d2)
K1 <- apply(D[,d1], 2, sum)
K2 <- apply(D[,d2], 2, sum)

plot(M, type="l", main="Simulated Time Series",
     xlab=paste("N = ", N, ", T = ", c, sep=""))
plot(M, type="l", main="Simulated Time Series",
     xlab=paste("N = ", N, ", T = ", c, sep=""))

plot(M, type="l", main="Original Data")
plot(K1, type="l", main="Subsample 1, T=100, N=10000")
plot(K2, type="l", main="Subsample 1, T=250, N=10000")

densM <- density(M)
densK1 <- density(K1)
densK2 <- density(K2)

plot(densM$x, densM$y, type="l", lwd=2,
     xlab=paste("N = ", t, ", Bandwidth = ",
                round(densM$bw, 5), sep=""), ylab="Density",
     xlim=c(min(densM$x, densK1$x, densK2$x),
            max(densM$x, densK1$x, densK2$x)))
lines(densK1$x, densK1$y, col="red")
lines(densK2$x, densK2$y, col="blue")

out.d <- cbind(rbind(mean(M), mean(K1), mean(K2)),
               rbind(sd(M), sd(K1), sd(K2)),
               rbind(min(M), min(K1), min(K2)),
               rbind(max(M), max(K1), max(K2)),
               rbind(median(M), median(K1), median(K2)),
               rbind(skewness(M), skewness(K1), skewness(K2)),
               rbind(kurtosis(M), kurtosis(K1), kurtosis(K2)))

write.csv(t(out.d), paste("out/2N", t, "t(out.d).csv", sep=""))

# then subsample events and not time
ds <- JL.predict.dim(NROW(D), 0.1)
d1 <- random.component.selection(d=ds, d.original=NROW(D))
d2 <- random.component.selection(d=100, d.original=NROW(D))
d1 <- sort(d1)
d2 <- sort(d2)
K1 <- apply(D[d1,], 2, sum)
K2 <- apply(D[d2,], 2, sum)

plot(M, type="l", main="Original Data")
plot(K1, type="l", main="Subsample 1, T=500, N=3685")
plot(K2, type="l", main="Subsample 2, T=500, N=100")

cur1 <- CUR(D, c=500, r=100, method="random")
cur2 <- CUR(D, c=500, r=100, method="top.scores")

c.ind <- cur1@C.index
r.ind <- cur1@R.index
K1 <- apply(D[r.ind ,c.ind], 2, sum)

c.ind <- cur2@C.index
r.ind <- cur2@R.index
K2 <- apply(D[r.ind ,c.ind], 2, sum)

plot(M, type="l", main="Original Data")
plot(K1, type="l", main="Subsample 1, Random")
plot(K2, type="l", main="Subsample 2, Top Scores")


