rm(list=ls(all=TRUE))
library("Matrix")
library("abind")
library("rugarch")
library("forecast")
library("lubridate")
setwd("output/")

# MAINOUT2-target.ei_isin_m.COUNTRYCO        MONTHLY  --  Industrial Production
# MAINOUT2-target.ei_lmhr_m.COUNTRYCO        MONTHLY  --  Unemployment Rate
# MAINOUT2-target.namq_10_a10_e.COUNTRYCO    QUARTERLY -  Employment in Thousand of Persons
# MAINOUT2-target.prc_hicp_midx.COUNTRYCO    MONTHLY  --  Harmonised CPI
# MAINOUT2-target.namq_10_gdp.COUNTRYCO      QUARTERLY -  GDP

# Industrial-Production
# Unemployment-Rate
# EmploymentTHP
# HCPI
# GDP

# 2) we will create the best strategy models
# 3) we will create model averaging
# and then create the best model strategy as well as the averaging
xfile <- "MAINOUT2-target.ei_isin_m.DE"
saveindir <- "output/DE/"
saveindir2 <- "output/DE/"
xsername <- "Industrial-Production"

load(paste("output/", xfile, ".Rdata",sep=""))

i <- 1

dataf <- NULL
datain5w <- list()
datain4w <- list()
datain3w <- list()
datain2w <- list()
datain1w <- list()

for(i in 1:NROW(tardates))
{
  foredate <- tardates[i]
  foredatew <- which.min(abs(dwF-(foredate+cT[2,target])))
  foredatew <- dwF[(foredatew-6):foredatew][1:6]
  
  dataf <- c(dataf, as.character(foredate))
  # for5w
  j <- 1; cur.date <- foredatew[j]; y <- as.matrix(Y[1:(which(dmF==foredate)),]); dor <- paste(min(year(foredatew)), "-", givemonth(min(month(foredatew))),"-", cT[2,target], sep=""); dor <- as.Date(dor); if(cur.date<dor){if(cT[1,target]<0){y[(NROW(y)+cT[1,target]):NROW(y),] <- NA}}else{if(cT[1,target]<0){y[(NROW(y)+cT[1,target]+1):NROW(y),] <- NA}}
  datain5w <- c(datain5w, list(y))
      
  j <- 2; cur.date <- foredatew[j]; y <- as.matrix(Y[1:(which(dmF==foredate)),]); dor <- paste(min(year(foredatew)), "-", givemonth(min(month(foredatew))),"-", cT[2,target], sep=""); dor <- as.Date(dor); if(cur.date<dor){if(cT[1,target]<0){y[(NROW(y)+cT[1,target]):NROW(y),] <- NA}}else{if(cT[1,target]<0){y[(NROW(y)+cT[1,target]+1):NROW(y),] <- NA}}
  datain4w <- c(datain4w, list(y))
  
  j <- 3; cur.date <- foredatew[j]; y <- as.matrix(Y[1:(which(dmF==foredate)),]); dor <- paste(min(year(foredatew)), "-", givemonth(min(month(foredatew))),"-", cT[2,target], sep=""); dor <- as.Date(dor); if(cur.date<dor){if(cT[1,target]<0){y[(NROW(y)+cT[1,target]):NROW(y),] <- NA}}else{if(cT[1,target]<0){y[(NROW(y)+cT[1,target]+1):NROW(y),] <- NA}}
  datain3w <- c(datain3w, list(y))
  
  j <- 4; cur.date <- foredatew[j]; y <- as.matrix(Y[1:(which(dmF==foredate)),]); dor <- paste(min(year(foredatew)), "-", givemonth(min(month(foredatew))),"-", cT[2,target], sep=""); dor <- as.Date(dor); if(cur.date<dor){if(cT[1,target]<0){y[(NROW(y)+cT[1,target]):NROW(y),] <- NA}}else{if(cT[1,target]<0){y[(NROW(y)+cT[1,target]+1):NROW(y),] <- NA}}
  datain2w <- c(datain2w, list(y))
  
  j <- 5; cur.date <- foredatew[j]; y <- as.matrix(Y[1:(which(dmF==foredate)),]); dor <- paste(min(year(foredatew)), "-", givemonth(min(month(foredatew))),"-", cT[2,target], sep=""); dor <- as.Date(dor); if(cur.date<dor){if(cT[1,target]<0){y[(NROW(y)+cT[1,target]):NROW(y),] <- NA}}else{if(cT[1,target]<0){y[(NROW(y)+cT[1,target]+1):NROW(y),] <- NA}}
  datain1w <- c(datain1w, list(y))
}

dataall <- matrix(NA, length(datain1w), 1+5)
colnames(dataall) <- c("TargetDate", "5w", "4w", "3w", "2w", "1w")

for(i in 1:length(datain1w))
{
  dataall[i,1] <- dataf[i]
  
  temp <- rownames(as.matrix(na.omit(datain5w[[i]])))
  dataall[i,2] <- paste(temp[1], "**",temp[NROW(temp)], sep="")
  
  temp <- rownames(as.matrix(na.omit(datain4w[[i]])))
  dataall[i,3] <- paste(temp[1], "**",temp[NROW(temp)], sep="")
  
  temp <- rownames(as.matrix(na.omit(datain3w[[i]])))
  dataall[i,4] <- paste(temp[1], "**",temp[NROW(temp)], sep="")
  
  temp <- rownames(as.matrix(na.omit(datain2w[[i]])))
  dataall[i,5] <- paste(temp[1], "**",temp[NROW(temp)], sep="")
  
  temp <- rownames(as.matrix(na.omit(datain1w[[i]])))
  dataall[i,6] <- paste(temp[1], "**",temp[NROW(temp)], sep="")
}





# Bind all methods and univ models
all5w <- abind(for5w, uni5w, along=3)
all4w <- abind(for4w, uni4w, along=3)
all3w <- abind(for3w, uni3w, along=3)
all2w <- abind(for2w, uni2w, along=3)
all1w <- abind(for1w, uni1w, along=3)
all0w <- abind(for0w, uni0w, along=3)

# Extract the point forecasts to create the errors
# Extract all the point forecasts
arow <- dim(all5w)[1]
acol <- dim(all5w)[3]
anams <- unlist(dimnames(all5w)[3])
for5p <- matrix(NA, arow, acol)
rownames(for5p) <- rownames(Y)
colnames(for5p) <- anams
for4p <- for5p
for3p <- for5p
for2p <- for5p
for1p <- for5p
for0p <- for5p
for(j in 1:acol)
{
  for5p[,anams[j]] <- all5w[,1,anams[j]]
  for4p[,anams[j]] <- all4w[,1,anams[j]]
  for3p[,anams[j]] <- all3w[,1,anams[j]]
  for2p[,anams[j]] <- all2w[,1,anams[j]]
  for1p[,anams[j]] <- all1w[,1,anams[j]]
  for0p[,anams[j]] <- all0w[,1,anams[j]]
}

for5p <- na.omit(for5p)
for4p <- na.omit(for4p)
for3p <- na.omit(for3p)
for2p <- na.omit(for2p)
for1p <- na.omit(for1p)
for0p <- na.omit(for0p)


forall <- cbind(for5p[,"Naive"], for4p[,"Naive"], for3p[,"Naive"], for2p[,"Naive"], for1p[,"Naive"])

write.csv(cbind(dataall, forall), "output/check.csv")





