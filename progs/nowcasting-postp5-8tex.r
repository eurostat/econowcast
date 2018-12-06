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
        file=paste("output/", xsername, "-", co, "-", xff, ".tex", sep=""))
}
# Industrial-Production
# Unemployment-Rate
# EmploymentTHP
# HCPI
# GDP

xsername <- "Industrial-Production"
co <- "DE"; xff <- "SSR";   xff2 <- "ssr";                    xlabl <- "dem1a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "Density-CR-90";   xff2 <- "pissrA-5-95";  xlabl <- "dem1b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "Density-CR-80";   xff2 <- "pissrB-10-90"; xlabl <- "dem1c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "Density-CR-60";   xff2 <- "pissrC-20-80"; xlabl <- "dem1d"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "LR";   xff2 <- "berk";                    xlabl <- "dem1e"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "SSR";   xff2 <- "ssr";                    xlabl <- "frm1a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "Density-CR-90";   xff2 <- "pissrA-5-95";  xlabl <- "frm1b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "Density-CR-80";   xff2 <- "pissrB-10-90"; xlabl <- "frm1c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "Density-CR-60";   xff2 <- "pissrC-20-80"; xlabl <- "frm1d"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "LR";   xff2 <- "berk";                    xlabl <- "frm1e"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "SSR";   xff2 <- "ssr";                    xlabl <- "itm1a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "Density-CR-90";   xff2 <- "pissrA-5-95";  xlabl <- "itm1b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "Density-CR-80";   xff2 <- "pissrB-10-90"; xlabl <- "itm1c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "Density-CR-60";   xff2 <- "pissrC-20-80"; xlabl <- "itm1d"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "LR";   xff2 <- "berk";                    xlabl <- "itm1e"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "SSR";   xff2 <- "ssr";                    xlabl <- "ukm1a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "Density-CR-90";   xff2 <- "pissrA-5-95";  xlabl <- "ukm1b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "Density-CR-80";   xff2 <- "pissrB-10-90"; xlabl <- "ukm1c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "Density-CR-60";   xff2 <- "pissrC-20-80"; xlabl <- "ukm1d"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "LR";   xff2 <- "berk";                    xlabl <- "ukm1e"; print.tex(xlabl, xff2, xff, co, xsername)


xsername <- "HCPI"
co <- "DE"; xff <- "SSR";   xff2 <- "ssr";                    xlabl <- "dem2a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "Density-CR-90";   xff2 <- "pissrA-5-95";  xlabl <- "dem2b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "Density-CR-80";   xff2 <- "pissrB-10-90"; xlabl <- "dem2c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "Density-CR-60";   xff2 <- "pissrC-20-80"; xlabl <- "dem2d"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "LR";   xff2 <- "berk";                    xlabl <- "dem2e"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "SSR";   xff2 <- "ssr";                    xlabl <- "frm2a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "Density-CR-90";   xff2 <- "pissrA-5-95";  xlabl <- "frm2b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "Density-CR-80";   xff2 <- "pissrB-10-90"; xlabl <- "frm2c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "Density-CR-60";   xff2 <- "pissrC-20-80"; xlabl <- "frm2d"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "LR";   xff2 <- "berk";                    xlabl <- "frm2e"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "SSR";   xff2 <- "ssr";                    xlabl <- "itm2a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "Density-CR-90";   xff2 <- "pissrA-5-95";  xlabl <- "itm2b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "Density-CR-80";   xff2 <- "pissrB-10-90"; xlabl <- "itm2c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "Density-CR-60";   xff2 <- "pissrC-20-80"; xlabl <- "itm2d"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "LR";   xff2 <- "berk";                    xlabl <- "itm2e"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "SSR";   xff2 <- "ssr";                    xlabl <- "ukm2a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "Density-CR-90";   xff2 <- "pissrA-5-95";  xlabl <- "ukm2b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "Density-CR-80";   xff2 <- "pissrB-10-90"; xlabl <- "ukm2c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "Density-CR-60";   xff2 <- "pissrC-20-80"; xlabl <- "ukm2d"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "LR";   xff2 <- "berk";                    xlabl <- "ukm2e"; print.tex(xlabl, xff2, xff, co, xsername)


xsername <- "Unemployment-Rate"
co <- "DE"; xff <- "SSR";   xff2 <- "ssr";                    xlabl <- "dem3a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "Density-CR-90";   xff2 <- "pissrA-5-95";  xlabl <- "dem3b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "Density-CR-80";   xff2 <- "pissrB-10-90"; xlabl <- "dem3c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "Density-CR-60";   xff2 <- "pissrC-20-80"; xlabl <- "dem3d"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "LR";   xff2 <- "berk";                    xlabl <- "dem3e"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "SSR";   xff2 <- "ssr";                    xlabl <- "frm3a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "Density-CR-90";   xff2 <- "pissrA-5-95";  xlabl <- "frm3b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "Density-CR-80";   xff2 <- "pissrB-10-90"; xlabl <- "frm3c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "Density-CR-60";   xff2 <- "pissrC-20-80"; xlabl <- "frm3d"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "LR";   xff2 <- "berk";                    xlabl <- "frm3e"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "SSR";   xff2 <- "ssr";                    xlabl <- "itm3a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "Density-CR-90";   xff2 <- "pissrA-5-95";  xlabl <- "itm3b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "Density-CR-80";   xff2 <- "pissrB-10-90"; xlabl <- "itm3c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "Density-CR-60";   xff2 <- "pissrC-20-80"; xlabl <- "itm3d"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "LR";   xff2 <- "berk";                    xlabl <- "itm3e"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "SSR";   xff2 <- "ssr";                    xlabl <- "ukm3a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "Density-CR-90";   xff2 <- "pissrA-5-95";  xlabl <- "ukm3b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "Density-CR-80";   xff2 <- "pissrB-10-90"; xlabl <- "ukm3c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "Density-CR-60";   xff2 <- "pissrC-20-80"; xlabl <- "ukm3d"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "LR";   xff2 <- "berk";                    xlabl <- "ukm3e"; print.tex(xlabl, xff2, xff, co, xsername)

xsername <- "GDP"
co <- "DE"; xff <- "SSR";   xff2 <- "ssr";                    xlabl <- "deq1a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "Density-CR-90";   xff2 <- "pissrA-5-95";  xlabl <- "deq1b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "Density-CR-80";   xff2 <- "pissrB-10-90"; xlabl <- "deq1c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "Density-CR-60";   xff2 <- "pissrC-20-80"; xlabl <- "deq1d"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "DE"; xff <- "LR";   xff2 <- "berk";                    xlabl <- "deq1e"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "SSR";   xff2 <- "ssr";                    xlabl <- "frq1a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "Density-CR-90";   xff2 <- "pissrA-5-95";  xlabl <- "frq1b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "Density-CR-80";   xff2 <- "pissrB-10-90"; xlabl <- "frq1c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "Density-CR-60";   xff2 <- "pissrC-20-80"; xlabl <- "frq1d"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "FR"; xff <- "LR";   xff2 <- "berk";                    xlabl <- "frq1e"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "SSR";   xff2 <- "ssr";                    xlabl <- "itq1a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "Density-CR-90";   xff2 <- "pissrA-5-95";  xlabl <- "itq1b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "Density-CR-80";   xff2 <- "pissrB-10-90"; xlabl <- "itq1c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "Density-CR-60";   xff2 <- "pissrC-20-80"; xlabl <- "itq1d"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "IT"; xff <- "LR";   xff2 <- "berk";                    xlabl <- "itq1e"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "SSR";   xff2 <- "ssr";                    xlabl <- "ukq1a"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "Density-CR-90";   xff2 <- "pissrA-5-95";  xlabl <- "ukq1b"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "Density-CR-80";   xff2 <- "pissrB-10-90"; xlabl <- "ukq1c"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "Density-CR-60";   xff2 <- "pissrC-20-80"; xlabl <- "ukq1d"; print.tex(xlabl, xff2, xff, co, xsername)
co <- "UK"; xff <- "LR";   xff2 <- "berk";                    xlabl <- "ukq1e"; print.tex(xlabl, xff2, xff, co, xsername)


