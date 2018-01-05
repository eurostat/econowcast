# Extract factors using:
#	XX: matrix of observed regressors
#	q: dynamic rank
#   r: static rank (r>=q)
#   p: ar order of the state vector
fac.out <- FactorExtraction(XX,q=fq,r=fr,p=fp)
Fac <- fac.out$Fac
rownames(Fac) <- rownames(XX)

# Then use the linreg with the factors
z <- YY; f <- Fac; vlag <- 0; 
source("./linreg.R")