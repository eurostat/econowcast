rm(list=ls(all=TRUE))
setwd("../data/bd/")

library("lubridate")
# library(devtools)
# devtools::install_github("PMassicotte/gtrendsR")
library("gtrendsR")

# Written by F Papalias
weekly.GOOGLE <- function(kwd, reg, cct, stp, dfrom, dto)
{
  # Load Large time frame
  tsd <- paste(dfrom, dto, sep=" ")            # Time Frame: "all" (since 2004),
  gt <- gtrends(kwd, reg, tsd, stp, cct)
  xL <- gt$interest_over_time$hits
  dL <- as.Date(gt$interest_over_time$date)
  
  # is it weekly?
  timediff <- dL[2:NROW(dL)] - dL[1:(NROW(dL)-1)]
  
  if(median(timediff)==7){
    cat(kwd, " - We already have weekly frequency, so no further action!", "\n")
  
    # Put it in a matrix wih appropriate row names
    xScaledF <- as.matrix(as.numeric(xL))
    colnames(xScaledF) <- kwd
    rownames(xScaledF) <- as.character(dL)
  }else{
    cat(kwd, " - We will collect weekly data, parse them and scale them!", "\n")
    
    # Break the dates in 5-year periods
    sngly <- unique(year(dL))
    fypf <- seq(sngly[1], sngly[NROW(sngly)], 5)
    fypt <- fypf+4
    if(max(fypt)>max(sngly)){
      fypt[which.max(fypt)] <- sngly[which.max(sngly)]
    }
    
    # Make them proper dates
    fypf <- as.Date(paste(fypf, "-01-01", sep=""))
    fypt <- as.Date(paste(fypt, "-12-31", sep=""))
    if(max(fypt)>=Sys.Date()){
      fypt[which.max(fypt)] <- Sys.Date()-1
    }
    if(min(fypf)<as.Date("2004-01-01")){
      fypf[which.min(fypf)] <- as.Date("2004-01-01")
    }
    fypf <- as.character(fypf)
    fypt <- as.character(fypt)
    
    # Collect the data and store it in lists
    xS <- NULL
    dS <- NULL
    
    for(i in 1:NROW(fypf))
    {
      cat(kwd, " - Now downloading ", i, "of ", NROW(fypf), " subperiods ", "\n")
      tsd <- paste(fypf[i], fypt[i], sep=" ")
      gt <- gtrends(kwd, reg, tsd, stp, cct)
      xtemp <- gt$interest_over_time$hits
      dtemp <- as.Date(gt$interest_over_time$date)
      
      xS <- c(xS, list(xtemp))
      dS <- c(dS, list(dtemp))
    }
    
    # Bind everything
    xZ <- xS[[1]]
    dZ <- dS[[1]]
    for(i in 2:NROW(fypf))
    {
      xZ <- c(xZ, xS[[i]])
      dZ <- c(dZ, dS[[i]])
    }
    
    # Select non-duplicates
    nondplc <- which(duplicated(dZ)==FALSE)
  
    # Make final selection of short data
    xF <- xZ[nondplc]
    dF <- dZ[nondplc]
    
    # Now, we must scale all weekly data according to the monthly ones
    # Identify the key dates and Calculate the factor
    dFmatch <- rep(NA, NROW(dF))
    for(i in 1:NROW(dF))
    {
      dFmatch[i] <- which.min(abs(dF[i]-dL))
    }
    
    # Check there are no zeros in the denoms later
    zcheck <- NULL
    for(i in 1:NROW(xF))
    {
      zcheck <- c(zcheck, which(dFmatch==dFmatch[i])[1])
    }
    zcheck <- unique(zcheck)
    
    xF2 <- xF
    # Check there are no zeros at the beginning
    if(xF2[zcheck[1]]==0){
      zer <- 1
      i <- 2
      while(xF2[zcheck[i]]==0){
        zer <- c(zer, i)
        i <- i +1
      }
      xF2[zcheck][zer] <- xF2[zcheck][i]
    }
    
    # Check there are no zeros inside
    for(i in 2:NROW(zcheck))
    {
      if(xF2[zcheck[i]]==0){
        xF2[zcheck[i]] <- xF2[zcheck[i-1]]
      }
    }
    
    xScaled <- rep(NA, NROW(xF))
    for(i in 1:NROW(xF))
    {
      xScaled[i] <- xL[dFmatch[i]]*xF2[i]/xF2[which(dFmatch==dFmatch[i])[1]]
    }
  
    # Before rescaling, make sure that we get similar results
    # xcheck <- rep(NA, NROW(xL))
    # for(i in 1:NROW(xL))
    # {
    #   xcheck[i] <- which(dFmatch==i)[1]
    # }
    # xcheck <- xScaled[xcheck]
    # plot(xL, type="l")
    # lines(xcheck, col="red")
    # 
    # par(mfrow=c(2,1))
    #   plot(dL, xL, type="l")
    #   plot(dF, xScaled, type="l")
    # par(mfrow=c(1,1))
    
    # Now, rescale everything to be 0-100
    xScaledF <- round((xScaled/max(xScaled))*100,2)
    
    # Put it in a matrix wih appropriate row names
    xScaledF <- as.matrix(xScaledF)
    colnames(xScaledF) <- paste(kwd, stp, reg, sep="-")
    rownames(xScaledF) <- as.character(dF)
  }
 return(xScaledF) 
}

# Set dates for all data
dfrom <- "2004-01-01"	# starting date
dto <- "2017-09-01"		# ending date

#  (the news doesn't really have a lot of data, so stick to the web)
stp <- "web"            # web; news; 
cct <- 0                # category

# General Indexes - web
reg <- ""               # Region, blank for all regions or use "GB" for the UK, etc.
kwd <- "uncertainty"

# Download the weekly trends
# Input: kwd (keyword), reg (region)
#		 cct (category), stp (domain)
#		 dfrom (start), dto (end)
c1 <- weekly.GOOGLE(kwd, reg, cct, stp, dfrom, dto)


kwd <- "economic uncertainty";    c2 <- weekly.GOOGLE(kwd, reg, cct, stp, dfrom, dto)
kwd <- "financial uncertainty";   c3 <- weekly.GOOGLE(kwd, reg, cct, stp, dfrom, dto)
kwd <- "policy uncertainty";      c4 <- weekly.GOOGLE(kwd, reg, cct, stp, dfrom, dto)

C <- cbind(c1, c2, c3, c4)
write.csv(C, "out/GOOGLE-general-uncertainty-web.csv")

# General Indexes - web
reg <- ""               # Region

kwd <- "risk";              c1 <- weekly.GOOGLE(kwd, reg, cct, stp, dfrom, dto)
kwd <- "financial risk";    c2 <- weekly.GOOGLE(kwd, reg, cct, stp, dfrom, dto)
kwd <- "political risk";    c3 <- weekly.GOOGLE(kwd, reg, cct, stp, dfrom, dto)
kwd <- "policy risk";       c4 <- weekly.GOOGLE(kwd, reg, cct, stp, dfrom, dto)

C <- cbind(c1, c2, c3, c4)
write.csv(C, "out/GOOGLE-general-risk-web.csv")

# France
reg <- "FR"               # Region
kwd <- "incertitude";         c1 <- weekly.GOOGLE(kwd, reg, cct, stp, dfrom, dto)
kwd <- "risque";              c2 <- weekly.GOOGLE(kwd, reg, cct, stp, dfrom, dto)
C <- cbind(c1, c2)
write.csv(C, "out/GOOGLE-FR.csv")

# DE
reg <- "DE"               # Region
kwd <- "unsicherheit";         c1 <- weekly.GOOGLE(kwd, reg, cct, stp, dfrom, dto)
kwd <- "risiko";              c2 <- weekly.GOOGLE(kwd, reg, cct, stp, dfrom, dto)
C <- cbind(c1, c2)
write.csv(C, "out/GOOGLE-DE.csv")

# IT
reg <- "IT"               # Region
kwd <- "incertezza";         c1 <- weekly.GOOGLE(kwd, reg, cct, stp, dfrom, dto)
kwd <- "rischio";              c2 <- weekly.GOOGLE(kwd, reg, cct, stp, dfrom, dto)
C <- cbind(c1, c2)
write.csv(C, "out/GOOGLE-IT.csv")

# GB
reg <- "GB"               # Region
kwd <- "uncertainty";         c1 <- weekly.GOOGLE(kwd, reg, cct, stp, dfrom, dto)
kwd <- "risk";              c2 <- weekly.GOOGLE(kwd, reg, cct, stp, dfrom, dto)
C <- cbind(c1, c2)
write.csv(C, "out/GOOGLE-GB.csv")


