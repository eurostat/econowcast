rm(list=ls(all=TRUE))
library("lubridate")
library("forecast")
library("MASS")
library("pls")
library("nsprcomp")
library("lars")
library("monomvn")
library("glmnet")
library("BoomSpikeSlab")

source("./aux-functions.R")

setwd("../data/bd/")

# Set the country
cocode <- "DE"
# target.ei_isin_m.COUNTRYCO        MONTHLY  --  Industrial Production
# target.ei_lmhr_m.COUNTRYCO        MONTHLY  --  Unemployment Rate
# target.namq_10_a10_e.COUNTRYCO    QUARTERLY -  Employment in Thousand of Persons
# target.prc_hicp_midx.COUNTRYCO    MONTHLY  --  Harmonised CPI
# target.namq_10_gdp.COUNTRYCO      QUARTERLY -  GDP
target <- "target.prc_hicp_midx.DE"

# Load the data
xw <- read.csv(paste("raw/", cocode, "/FINAL-WEEKLY.csv", sep=""))
xm <- read.csv(paste("raw/", cocode, "/FINAL-MONTHLY.csv", sep=""))
xT <- read.csv(paste("raw/", cocode, "/FINAL-TARGETS.csv", sep=""))

xw <- stripatts(xw); cw <- xw$atts; xw <- xw$data; dw <- as.Date(rownames(xw))
xm <- stripatts(xm); cm <- xm$atts; xm <- xm$data; dm <- as.Date(rownames(xm))
xT <- stripatts(xT); cT <- xT$atts; xT <- xT$data; dT <- as.Date(rownames(xT))

Y <- as.matrix(xT[,target])
Yd <- as.Date(rownames(Y))

# Also inlude the other monthly variables as predictors
ino <- c(which(colnames(xT)==target), 3, 5)
pT <- xT[,-ino]

# And put everything together
xmF <- cbind(xm, pT)
cmF <- cbind(cm, cT[,-ino])
dmF <- as.Date(rownames(xmF))

# Extract Google
xG <- xw[,1:2]; cG <- cw[,1:2]

# Extract Reuters
npst1 <- paste("reuters.", cocode, sep="")
xR <- as.matrix(xw[,npst1]); cR <- as.matrix(cw[,npst1])

# Extract Financials
xF <- xw[,c(3:15, 17)]; cF <- cw[,c(3:15, 17)]

# Extract Bonds
xB <- xw[, 18:48]; cB <- cw[, 18:48]

# Create final XB
isel <- c(1,2,6,11,31)
xBF <- cbind(xB[,isel], xB[,11]-xB[,1], xB[,11]-xB[,3],
                                  xB[,31]-xB[,1], xB[,31]-xB[,3])
cBF <- cbind(cB[,isel], matrix(c(0,0,1),3,4))
colnames(xBF) <- c(colnames(xB)[isel], "10y-6m", "10y-2y", "30-6m", "30-2y")

# Bind again together
xwF <- cbind(xF, xBF)
cwF <- cbind(cF, cBF)

colnames(xwF) <- c(colnames(xF), colnames(xBF))
dwF <- as.Date(rownames(xwF))

# Export final selection
write.csv(colnames(xwF), paste("out/FINAL-WEEKLY-SEL-", target, ".csv",sep=""))
write.csv(colnames(xmF), paste("out/FINAL-MONTHLY-SEL-", target, ".csv",sep=""))

# All target dates
tardates <- Yd[which(Yd==as.Date("2014-01-31")):NROW(Yd)]

mnam.simple <- c("Ave4", "Ave12", "Ave24", "Naive", "AR(1)", "AR(4)", "AR(AIC)")
mnam.reg <- c("Google", "Reuters", "Google-L1", "Reuters-L1", "Google-L3", "Reuters-L3",
              "DFA2-MacroFin", "DFA3-MacroFin", "DFA4-MacroFin", "DFA5-MacroFin",
              "DFA2-MacroFin-Google", "DFA3-MacroFin-Google", "DFA4-MacroFin-Google", "DFA5-MacroFin-Google",
              "DFA2-MacroFin-Reuters", "DFA3-MacroFin-Reuters", "DFA4-MacroFin-Reuters", "DFA5-MacroFin-Reuters",
              "DFA2-MacroFin-GoogleReuters", "DFA3-MacroFin-GoogleReuters", "DFA4-MacroFin-GoogleReuters",
              "DFA5-MacroFin-GoogleReuters")
mnam.new <- c("PLS(1)", "PLS(2)", "PLS(3)", "PLS(4)", "PLS(5)",
              "SPC(1)", "SPC(2)", "SPC(3)", "SPC(4)", "SPC(5)",
              "LASSO", "LAR", "Spike")
mnam1 <- paste(mnam.new, "-MacroFin", sep="")
mnam2 <- paste(mnam.new, "-MacroFin-Google", sep="")
mnam3 <- paste(mnam.new, "-MacroFin-Reuters", sep="")
mnam4 <- paste(mnam.new, "-MacroFin-GoogleReuters", sep="")
mnams <- c(mnam.simple, mnam.reg, mnam1, mnam2, mnam3, mnam4)

pintv <- seq(0, 1, 0.01)
B <- 200   ## bootstrap reps for CIs

for5w <- array(NA, c(NROW(Y), NROW(pintv)+1, NROW(mnams)),
               dimnames=list(rownames(Y), c("PointF", as.character(pintv)), mnams))
for4w <- for5w
for3w <- for5w
for2w <- for5w
for1w <- for5w
for0w <- for5w

# forecasts <- matrix(NA, NROW(Y), 6)
# colnames(forecasts) <- c("-5w","-4w","-3w", "-2w", "-1w", "0w")
# rownames(forecasts) <- rownames(Y)

for(i in 1:NROW(tardates))
{
foredate <- tardates[i]
foredatew <- which.min(abs(dwF-(foredate+cT[2,target])))
foredatew <- dwF[(foredatew-6):foredatew][1:6]

  # for5w
  j <- 1; cur.date <- foredatew[j]; source("E:/Dropbox/BD2/Tasks567/progs/for5w.R")
  j <- 2; cur.date <- foredatew[j]; source("E:/Dropbox/BD2/Tasks567/progs/for4w.R")
  j <- 3; cur.date <- foredatew[j]; source("E:/Dropbox/BD2/Tasks567/progs/for3w.R")
  j <- 4; cur.date <- foredatew[j]; source("E:/Dropbox/BD2/Tasks567/progs/for2w.R")
  j <- 5; cur.date <- foredatew[j]; source("E:/Dropbox/BD2/Tasks567/progs/for1w.R")
  j <- 6; cur.date <- foredatew[j]; source("E:/Dropbox/BD2/Tasks567/progs/for0w.R")
  cat("now done ", i, " of ", NROW(tardates), "\n")
}
  
# Extract all the point forecasts 
for5p <- matrix(NA, NROW(Y), NROW(mnams))
rownames(for5p) <- rownames(Y)
colnames(for5p) <- mnams
for4p <- for5p
for3p <- for5p
for2p <- for5p
for1p <- for5p
for0p <- for5p
for(j in 1:NROW(mnams))
{
  for5p[,j] <- for5w[,1,mnams[j]]
  for4p[,j] <- for4w[,1,mnams[j]]
  for3p[,j] <- for3w[,1,mnams[j]]
  for2p[,j] <- for2w[,1,mnams[j]]
  for1p[,j] <- for1w[,1,mnams[j]]
  for0p[,j] <- for0w[,1,mnams[j]]
}

for5p <- na.omit(for5p)
for4p <- na.omit(for4p)
for3p <- na.omit(for3p)
for2p <- na.omit(for2p)
for1p <- na.omit(for1p)
for0p <- na.omit(for0p)

W <- as.matrix(Y[(NROW(Y)-NROW(for5p)+1):NROW(Y),])
Wd <- as.Date(rownames(W))

# Check how it evolves from week to week
j <- 80
plot(Wd, W, type="l", lwd=2)
lines(Wd, for5p[,j], col="blue")
lines(Wd, for4p[,j], col="blue")
lines(Wd, for3p[,j], col="blue")
lines(Wd, for2p[,j], col="blue")
lines(Wd, for1p[,j], col="blue")
lines(Wd, for0p[,j], col="blue")


beep(10)
save.image(paste("out/MAINOUT-",target, ".Rdata", sep=""))
