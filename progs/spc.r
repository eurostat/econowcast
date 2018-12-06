vlag <- 1
if(vlag>0){
  # XX <- lagmv(XX, vlag)   # no because of small T dimension
  XX <- cbind(lagf(YY, vlag)[,2:(vlag+1)], XX)
  
  XX <- as.matrix(XX[(vlag+1):NROW(XX),])
  YY <- as.matrix(YY[(vlag+1):NROW(YY),])
}

xxin <- xstd(XX)
ymu <- mean(YY)
ysd <- sd(YY)
yyin <- (YY-ymu)/ysd

pc.out <- nsprcomp(x=xxin, retx=TRUE, ncomp=qncomp, nneg = FALSE,
                   center=FALSE, scale.=FALSE)
f <- as.matrix(pc.out$x)
z <- as.matrix(yyin)

source("Flinreg.R")