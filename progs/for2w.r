# Get the data
source("data-prep.R")
ys <- as.matrix(ys[(nas+1):NROW(ys),])
vws <- as.matrix(vws[(nas+1):NROW(vws),])
vGs <- as.matrix(vGs[(nas+1):NROW(vGs),])
vRs <- as.matrix(vRs[(nas+1):NROW(vRs),])
vms <- as.matrix(vms[(nas+1):NROW(vms),])

# remove outliers
vws <- apply(vws, 2, outliers_correction)
vGs <- apply(vGs, 2, outliers_correction)
vRs <- apply(vRs, 2, outliers_correction)
vms <- apply(vms, 2, outliers_correction)
vMF <- cbind(vws, vms)

YTRANSF <- cT[3,target]

imodel <- "Ave4"; z <- ys; zp <- 4; source("averaging.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "Ave12"; z <- ys; zp <- 12; source("averaging.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "Ave24"; z <- ys; zp <- 24; source("averaging.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "Naive"; z <- ys; source("naive.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "AR(1)"; z <- ys; arp <- 1; source("ar.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "AR(4)"; z <- ys; arp <- 4; source("ar.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "AR(AIC)"; z <- ys; arp <- 0; source("ar.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "Google"; z <- ys; f <- vGs; vlag <- 0; source("linreg.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "Reuters"; z <- ys; f <- vRs; vlag <- 0; source("linreg.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "Google-L1"; z <- ys; f <- vGs; vlag <- 1; source("linreg.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "Reuters-L1"; z <- ys; f <- vRs; vlag <- 1; source("linreg.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "Google-L3"; z <- ys; f <- vGs; vlag <- 3; source("linreg.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "Reuters-L3"; z <- ys; f <- vRs; vlag <- 3; source("linreg.R")
for2w[as.character(foredate), ,imodel] <- zout

# fq: dynamic rank
# fr: static rank (r>=q)
# fp: ar order of the state vector
imodel <- "DFA2-MacroFin"; fq <- 2;  fr <- fq; fp <- 1; XX <- vMF; YY <- ys
source("dfa.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "DFA3-MacroFin"; fq <- 3;  fr <- fq; fp <- 1; XX <- vMF; YY <- ys
source("dfa.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "DFA4-MacroFin"; fq <- 4;  fr <- fq; fp <- 1; XX <- vMF; YY <- ys
source("dfa.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "DFA5-MacroFin"; fq <- 5;  fr <- fq; fp <- 1; XX <- vMF; YY <- ys
source("dfa.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "DFA2-MacroFin-Google"; fq <- 2;  fr <- fq; fp <- 1; XX <- cbind(vMF, vGs); YY <- ys
source("dfa.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "DFA3-MacroFin-Google"; fq <- 3;  fr <- fq; fp <- 1; XX <- cbind(vMF, vGs); YY <- ys
source("dfa.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "DFA4-MacroFin-Google"; fq <- 4;  fr <- fq; fp <- 1; XX <- cbind(vMF, vGs); YY <- ys
source("dfa.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "DFA5-MacroFin-Google"; fq <- 5;  fr <- fq; fp <- 1; XX <- cbind(vMF, vGs); YY <- ys
source("dfa.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "DFA2-MacroFin-Reuters"; fq <- 2;  fr <- fq; fp <- 1; XX <- cbind(vMF, vRs); YY <- ys
source("dfa.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "DFA3-MacroFin-Reuters"; fq <- 3;  fr <- fq; fp <- 1; XX <- cbind(vMF, vRs); YY <- ys
source("dfa.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "DFA4-MacroFin-Reuters"; fq <- 4;  fr <- fq; fp <- 1; XX <- cbind(vMF, vRs); YY <- ys
source("dfa.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "DFA5-MacroFin-Reuters"; fq <- 5;  fr <- fq; fp <- 1; XX <- cbind(vMF, vRs); YY <- ys
source("dfa.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "DFA2-MacroFin-GoogleReuters"; fq <- 2;  fr <- fq; fp <- 1; XX <- cbind(vMF, vGs, vRs); YY <- ys
source("dfa.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "DFA3-MacroFin-GoogleReuters"; fq <- 3;  fr <- fq; fp <- 1; XX <- cbind(vMF, vGs, vRs); YY <- ys
source("dfa.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "DFA4-MacroFin-GoogleReuters"; fq <- 4;  fr <- fq; fp <- 1; XX <- cbind(vMF, vGs, vRs); YY <- ys
source("dfa.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "DFA5-MacroFin-GoogleReuters"; fq <- 5;  fr <- fq; fp <- 1; XX <- cbind(vMF, vGs, vRs); YY <- ys
source("dfa.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(1)-MacroFin"; XX <- vMF; YY <- ys; qncomp <- 1; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(2)-MacroFin"; XX <- vMF; YY <- ys; qncomp <- 2; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(3)-MacroFin"; XX <- vMF; YY <- ys; qncomp <- 3; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(4)-MacroFin"; XX <- vMF; YY <- ys; qncomp <- 4; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(5)-MacroFin"; XX <- vMF; YY <- ys; qncomp <- 5; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(1)-MacroFin-Google"; XX <- cbind(vMF, vGs); YY <- ys; qncomp <- 1; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(2)-MacroFin-Google"; XX <- cbind(vMF, vGs); YY <- ys; qncomp <- 2; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(3)-MacroFin-Google"; XX <- cbind(vMF, vGs); YY <- ys; qncomp <- 3; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(4)-MacroFin-Google"; XX <- cbind(vMF, vGs); YY <- ys; qncomp <- 4; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(5)-MacroFin-Google"; XX <- cbind(vMF, vGs); YY <- ys; qncomp <- 5; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(1)-MacroFin-Reuters"; XX <- cbind(vMF, vRs); YY <- ys; qncomp <- 1; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(2)-MacroFin-Reuters"; XX <- cbind(vMF, vRs); YY <- ys; qncomp <- 2; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(3)-MacroFin-Reuters"; XX <- cbind(vMF, vRs); YY <- ys; qncomp <- 3; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(4)-MacroFin-Reuters"; XX <- cbind(vMF, vRs); YY <- ys; qncomp <- 4; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(5)-MacroFin-Reuters"; XX <- cbind(vMF, vRs); YY <- ys; qncomp <- 5; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(1)-MacroFin-GoogleReuters"; XX <- cbind(vMF, vGs, vRs); YY <- ys; qncomp <- 1; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(2)-MacroFin-GoogleReuters"; XX <- cbind(vMF, vGs, vRs); YY <- ys; qncomp <- 2; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(3)-MacroFin-GoogleReuters"; XX <- cbind(vMF, vGs, vRs); YY <- ys; qncomp <- 3; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(4)-MacroFin-GoogleReuters"; XX <- cbind(vMF, vGs, vRs); YY <- ys; qncomp <- 4; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "PLS(5)-MacroFin-GoogleReuters"; XX <- cbind(vMF, vGs, vRs); YY <- ys; qncomp <- 5; source("pls.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(1)-MacroFin"; XX <- vMF; YY <- ys; qncomp <- 1; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(2)-MacroFin"; XX <- vMF; YY <- ys; qncomp <- 2; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(3)-MacroFin"; XX <- vMF; YY <- ys; qncomp <- 3; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(4)-MacroFin"; XX <- vMF; YY <- ys; qncomp <- 4; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(5)-MacroFin"; XX <- vMF; YY <- ys; qncomp <- 5; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(1)-MacroFin-Google"; XX <- cbind(vMF, vGs); YY <- ys; qncomp <- 1; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(2)-MacroFin-Google"; XX <- cbind(vMF, vGs); YY <- ys; qncomp <- 2; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(3)-MacroFin-Google"; XX <- cbind(vMF, vGs); YY <- ys; qncomp <- 3; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(4)-MacroFin-Google"; XX <- cbind(vMF, vGs); YY <- ys; qncomp <- 4; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(5)-MacroFin-Google"; XX <- cbind(vMF, vGs); YY <- ys; qncomp <- 5; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(1)-MacroFin-Reuters"; XX <- cbind(vMF, vRs); YY <- ys; qncomp <- 1; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(2)-MacroFin-Reuters"; XX <- cbind(vMF, vRs); YY <- ys; qncomp <- 2; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(3)-MacroFin-Reuters"; XX <- cbind(vMF, vRs); YY <- ys; qncomp <- 3; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(4)-MacroFin-Reuters"; XX <- cbind(vMF, vRs); YY <- ys; qncomp <- 4; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(5)-MacroFin-Reuters"; XX <- cbind(vMF, vRs); YY <- ys; qncomp <- 5; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(1)-MacroFin-GoogleReuters"; XX <- cbind(vMF, vGs, vRs); YY <- ys; qncomp <- 1; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(2)-MacroFin-GoogleReuters"; XX <- cbind(vMF, vGs, vRs); YY <- ys; qncomp <- 2; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(3)-MacroFin-GoogleReuters"; XX <- cbind(vMF, vGs, vRs); YY <- ys; qncomp <- 3; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(4)-MacroFin-GoogleReuters"; XX <- cbind(vMF, vGs, vRs); YY <- ys; qncomp <- 4; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "SPC(5)-MacroFin-GoogleReuters"; XX <- cbind(vMF, vGs, vRs); YY <- ys; qncomp <- 5; source("spc.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "LASSO-MacroFin"; XX <- vMF; YY <- ys; salpha <- 1; source("sparse.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "LASSO-MacroFin-Google"; XX <- cbind(vMF, vGs); YY <- ys; salpha <- 1; source("sparse.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "LASSO-MacroFin-Reuters"; XX <- cbind(vMF, vRs); YY <- ys; salpha <- 1; source("sparse.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "LASSO-MacroFin-GoogleReuters"; XX <- cbind(vMF, vGs, vRs); YY <- ys; salpha <- 1; source("sparse.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "LAR-MacroFin"; XX <- vMF; YY <- ys; salpha <- 0.5; source("sparse.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "LAR-MacroFin-Google"; XX <- cbind(vMF, vGs); YY <- ys; salpha <- 0.5; source("sparse.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "LAR-MacroFin-Reuters"; XX <- cbind(vMF, vRs); YY <- ys; salpha <- 0.5; source("sparse.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "LAR-MacroFin-GoogleReuters"; XX <- cbind(vMF, vGs, vRs); YY <- ys; salpha <- 0.5; source("sparse.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "Spike-MacroFin"; XX <- vMF; YY <- ys; niters <- B*2; source("spike.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "Spike-MacroFin-Google"; XX <- cbind(vMF, vGs); YY <- ys; niters <- B*2; source("spike.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "Spike-MacroFin-Reuters"; XX <- cbind(vMF, vRs); YY <- ys; niters <- B*2; source("spike.R")
for2w[as.character(foredate), ,imodel] <- zout

imodel <- "Spike-MacroFin-GoogleReuters"; XX <- cbind(vMF, vGs, vRs); YY <- ys; niters <- B*2; source("spike.R")
for2w[as.character(foredate), ,imodel] <- zout