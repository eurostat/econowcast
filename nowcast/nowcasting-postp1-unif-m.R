rm(list=ls(all=TRUE))
library("forecast")
library("lubridate")
setwd("output/")

# in this program we load the forecasts
# 1) we will do the simpe univariate models on the level series
# 2) we will create the best strategy models
# 3) we will create model averaging
# and then create the best model strategy as well as the averaging

xfile <- "MAINOUT2-target.ei_isin_m.DE"

load(paste("output/", xfile, ".Rdata",sep=""))

# Do the univariate forecasts for the following dates
uD <- as.Date(rownames(for5p))
unams <- c("AutoArima", "baggedETS", "bats", "ets", "nnetar", "tbats", "theta", "spline")

uni5w <- array(NA, c(NROW(Y), NROW(pintv)+1, NROW(unams)),
               dimnames=list(rownames(Y), c("PointF", as.character(pintv)), unams))
uni4w <- uni5w
uni3w <- uni5w
uni2w <- uni5w
uni1w <- uni5w
uni0w <- uni5w

for(i in 1:NROW(uD))
{
  foredate <- uD[i]
  foredatew <- which.min(abs(dwF-(foredate+cT[2,target])))
  foredatew <- dwF[(foredatew-6):foredatew][1:6]
  
  j <- 1; cur.date <- foredatew[j]; source("unif-tar-m.R"); source("unif5w.R")
  j <- 2; cur.date <- foredatew[j]; source("unif-tar-m.R"); source("unif4w.R")
  j <- 3; cur.date <- foredatew[j]; source("unif-tar-m.R"); source("unif3w.R")
  j <- 4; cur.date <- foredatew[j]; source("unif-tar-m.R"); source("unif2w.R")
  j <- 5; cur.date <- foredatew[j]; source("unif-tar-m.R"); source("unif1w.R")
  j <- 6; cur.date <- foredatew[j]; source("unif-tar-m.R"); source("unif0w.R")
  cat("now done ", i, " out of ", NROW(uD), "\n")  
}



save.image(paste("MAINOUT2-",target, ".Rdata", sep=""))





