rm(list=ls(all=TRUE))
setwd("output/")
library("xtable")

get.mat <- function(dir, xf)
{
  z <- read.csv(paste(dir, xf, sep=""), header=TRUE)
  zn <- z[,1]
  z <- z[,2:NCOL(z)]
  z <- apply(z, 2, as.numeric)
  rownames(z) <- zn
  # Change the rownames
  znr <- c(7,5,6,42,1,2,3,4,30,8,9,92,43,91,93,31,32,33,64,65,66,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,38,39,40,41,34,35,36,37,87,88,89,90,10,12,13,11)
  nrnms <- c("Average(4)",	"Average(12)",	"Average(24)",	"Naive",	"AR(1)",	"AR(4)",	"AR(AIC)",	"AutoArima",	"ETS",	"BaggedETS",	"BATS",	"TBATS",	"NN",	"Spline",	"THETA",	"Google",	"Google-L1",	"Google-L3",	"Reuters",	"Reuters-L1",	"Reuters-L3",	"DFA(2)-MacroFin",	"DFA(2)-MacroFin-Google",	"DFA(2)-MacroFin-GoogleReuters",	"DFA(2)-MacroFin-Reuters",	"DFA(3)-MacroFin",	"DFA(3)-MacroFin-Google",	"DFA(3)-MacroFin-GoogleReuters",	"DFA(3)-MacroFin-Reuters",	"DFA(4)-MacroFin",	"DFA(4)-MacroFin-Google",	"DFA(4)-MacroFin-GoogleReuters",	"DFA(4)-MacroFin-Reuters",	"DFA(5)-MacroFin",	"DFA(5)-MacroFin-Google",	"DFA(5)-MacroFin-GoogleReuters",	"DFA(5)-MacroFin-Reuters",	"PLS(1)-MacroFin",	"PLS(1)-MacroFin-Google",	"PLS(1)-MacroFin-GoogleReuters",	"PLS(1)-MacroFin-Reuters",	"PLS(2)-MacroFin",	"PLS(2)-MacroFin-Google",	"PLS(2)-MacroFin-GoogleReuters",	"PLS(2)-MacroFin-Reuters",	"PLS(3)-MacroFin",	"PLS(3)-MacroFin-Google",	"PLS(3)-MacroFin-GoogleReuters",	"PLS(3)-MacroFin-Reuters",	"PLS(4)-MacroFin",	"PLS(4)-MacroFin-Google",	"PLS(4)-MacroFin-GoogleReuters",	"PLS(4)-MacroFin-Reuters",	"PLS(5)-MacroFin",	"PLS(5)-MacroFin-Google",	"PLS(5)-MacroFin-GoogleReuters",	"PLS(5)-MacroFin-Reuters",	"SPC(1)-MacroFin",	"SPC(1)-MacroFin-Google",	"SPC(1)-MacroFin-GoogleReuters",	"SPC(1)-MacroFin-Reuters",	"SPC(2)-MacroFin",	"SPC(2)-MacroFin-Google",	"SPC(2)-MacroFin-GoogleReuters",	"SPC(2)-MacroFin-Reuters",	"SPC(3)-MacroFin",	"SPC(3)-MacroFin-Google",	"SPC(3)-MacroFin-GoogleReuters",	"SPC(3)-MacroFin-Reuters",	"SPC(4)-MacroFin",	"SPC(4)-MacroFin-Google",	"SPC(4)-MacroFin-GoogleReuters",	"SPC(4)-MacroFin-Reuters",	"SPC(5)-MacroFin",	"SPC(5)-MacroFin-Google",	"SPC(5)-MacroFin-GoogleReuters",	"SPC(5)-MacroFin-Reuters",	"LASSO-MacroFin",	"LASSO-MacroFin-Google",	"LASSO-MacroFin-GoogleReuters",	"LASSO-MacroFin-Reuters",	"EN-MacroFin",	"EN-MacroFin-Google",	"EN-MacroFin-GoogleReuters",	"EN-MacroFin-Reuters",	"SSlab-MacroFin",	"SSlab-MacroFin-Google",	"SSlab-MacroFin-GoogleReuters",	"SSlab-MacroFin-Reuters",	"Best1",	"Best3",	"Best5",	"Best10")
  z <- round(z[znr,],3)
  z <- rbind(z, rep("", 6))
  # rownames(z) <- c(nrnms, " ")
  rownames(z) <- NULL
  z <- z[,1:5]
  zz <- cbind(nrnms[1:47], z[1:47,], c(nrnms[48:NROW(nrnms)], " "), z[48:NROW(z),])
  colnames(zz) <- rep(c("Model", "-5w", "-4w", "-3w", "-2w", "-1w"), 2)
  return(zz)
}
print.tex <- function(xlabl, xff2, xff, co, xsername)
{
  dir <- paste("output/", co, sep="")
  zzlab <- xlabl
  xf <- paste(xsername, "-", xff2, ".csv", sep="")
  zz <- get.mat(dir, xf)
  xx <- xtable(zz, caption=paste(co, xsername, xff, sep=", "), label=zzlab)
  print(xx, floating=TRUE, caption.placement="bottom",
        tabular.environment="tabular",
        floating.environment = "sidewaystable",
        include.rownames=FALSE, include.colnames=TRUE,
        scalebox = 0.6,
        file=paste("output/tables/R/", xsername, "-", co, "-", xff, ".tex", sep=""))
}
# Industrial-Production
# Unemployment-Rate
# EmploymentTHP
# HCPI
# GDP

xsername <- "Industrial-Production"
co <- "DE"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "dem1a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "dem1b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "DM";    xff2 <- "dm";     xlabl <- "dem1c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "frm1a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "frm1b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "DM";    xff2 <- "dm";     xlabl <- "frm1c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "itm1a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "itm1b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "DM";    xff2 <- "dm";     xlabl <- "itm1c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "ukm1a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "ukm1b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "DM";    xff2 <- "dm";     xlabl <- "ukm1c"; print.tex(xlabl, xff2, xff, co, xsername)


xsername <- "HCPI"
co <- "DE"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "dem2a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "dem2b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "DM";    xff2 <- "dm";     xlabl <- "dem2c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "frm2a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "frm2b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "DM";    xff2 <- "dm";     xlabl <- "frm2c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "itm2a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "itm2b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "DM";    xff2 <- "dm";     xlabl <- "itm2c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "ukm2a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "ukm2b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "DM";    xff2 <- "dm";     xlabl <- "ukm2c"; print.tex(xlabl, xff2, xff, co, xsername)


xsername <- "EmploymentTHP"
co <- "DE"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "dem3a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "dem3b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "DM";    xff2 <- "dm";     xlabl <- "dem3c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "frm3a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "frm3b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "DM";    xff2 <- "dm";     xlabl <- "frm3c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "itm3a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "itm3b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "DM";    xff2 <- "dm";     xlabl <- "itm3c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "ukm3a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "ukm3b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "DM";    xff2 <- "dm";     xlabl <- "ukm3c"; print.tex(xlabl, xff2, xff, co, xsername)


xsername <- "Unemployment-Rate"
co <- "DE"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "dem4a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "dem4b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "DM";    xff2 <- "dm";     xlabl <- "dem4c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "frm4a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "frm4b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "DM";    xff2 <- "dm";     xlabl <- "frm4c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "itm4a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "itm4b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "DM";    xff2 <- "dm";     xlabl <- "itm4c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "ukm4a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "ukm4b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "DM";    xff2 <- "dm";     xlabl <- "ukm4c"; print.tex(xlabl, xff2, xff, co, xsername)


xsername <- "GDP"
co <- "DE"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "deq1a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "deq1b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "DM";    xff2 <- "dm";     xlabl <- "deq1c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "frq1a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "frq1b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "DM";    xff2 <- "dm";     xlabl <- "frq1c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "itq1a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "itq1b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "DM";    xff2 <- "dm";     xlabl <- "itq1c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "MAE";   xff2 <- "mae";    xlabl <- "ukq1a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "RMSFE"; xff2 <- "rmsfe";  xlabl <- "ukq1b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "DM";    xff2 <- "dm";     xlabl <- "ukq1c"; print.tex(xlabl, xff2, xff, co, xsername)



