fac.out <- FactorExtraction(XX,q=fq,r=fr,p=fp)
Fac <- fac.out$Fac
rownames(Fac) <- rownames(XX)
z <- YY; f <- Fac; vlag <- 0; source("linreg.R")