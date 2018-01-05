rm(list=ls(all=TRUE))
setwd("../data/uncertainty/")
library("lubridate")

# Load the Reuters uncertainty indexes
x <- read.csv("./raw/reuters.csv", header=TRUE)
d <- as.Date(x[,1])
x <- x[,2:NCOL(x)]
x <- as.matrix(x)
x <- apply(x, 2, as.numeric)

# Construct the weekly indexes averaging out
xw <- matrix(NA, 0, 6)
dw <- seq(as.Date("2007-01-07"), d[NROW(d)], 7)

for(i in 1:NROW(dw))
{
  ii <- which(d==dw[i])
  xtemp <- x[(ii-7):(ii-1),]
  xw <- rbind(xw, colMeans(xtemp))
}

# Construct the monthly time series
m <-  unique(month(d))
y <- unique(year(d))

xm <- matrix(NA, 0, 6)
dm <- NULL

for(i in y[1]:y[NROW(y)])
{
  xsel <- which(year(d)==i)
  xtemp <- x[xsel,]
  dtemp <- d[xsel]

  for(ii in m[1]:m[NROW(m)])
  {
    xsel2 <- which(month(dtemp)==ii)
    if(NROW(xsel2)==0){
      next
    }else{
      xtemp2 <- xtemp[xsel2,]
      dtemp2 <- dtemp[xsel2]

      if(NROW(xsel2)==1)
      {
        xm <- rbind(xm, xtemp2)
      }else{
        xm <- rbind(xm, colMeans(xtemp2))
      }
      dm <- c(dm, as.character(dtemp2[NROW(dtemp2)]+1))
    }
  }
}

rownames(xw) <- as.character(dw)
rownames(xm) <- as.character(dm)

# remove the last observation in months
# because the month is not complete
xm <- xm[1:(NROW(xm)-1),]

# Export the average data
write.csv(xw[,1], "out/w1_UI.csv")
write.csv(xw[,2], "out/w2_RI.csv")
write.csv(xw[,3], "out/w3_FR.csv")
write.csv(xw[,4], "out/w4_DE.csv")
write.csv(xw[,5], "out/w5_IT.csv")
write.csv(xw[,6], "out/w6_UK.csv")

write.csv(xm[,1], "out/m1_UI.csv")
write.csv(xm[,2], "out/m2_RI.csv")
write.csv(xm[,3], "out/m3_FR.csv")
write.csv(xm[,4], "out/m4_DE.csv")
write.csv(xm[,5], "out/m5_IT.csv")
write.csv(xm[,6], "out/m6_UK.csv")

write.csv(xm, "raw/mreuters.csv")
write.csv(xw, "raw/wreuters.csv")
