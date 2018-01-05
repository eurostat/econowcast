rm(list=ls(all=TRUE))
setwd("../data/uncertainty/")

library("lubridate")
# library(devtools)
# devtools::install_github("PMassicotte/gtrendsR")
library("gtrendsR")
library("outliers")

# Lagging variables
lagf <- function(x,max.lag) embed(c(rep(NA,max.lag), x), max.lag+1)

# Outliers c sd away from the mean
away.mean <- function(z, c)
{
  ub <- mean(z)+c*sd(z)
  lb <- mean(z)-c*sd(z)
  out <- (z>ub)|(z<lb)
  return(out)
}

# Load the reuters data
xmr <- read.csv("raw/mreuters.csv", header=TRUE)
dmr <- as.Date(as.character(xmr[,1]))
xmr <- as.matrix(xmr[,2:NCOL(xmr)])

# select series to use
su <- 4
mainkwd <- "Uncertainty DE"

# Set the Google
kwd1 <- "unsicherheit"
kwd2 <- "risiko"
reg <- "DE"               # Region
tsd <- "all"            # Time Frame: "all" (since 2004),
cct <- 0                # category

kwd <- kwd1
stp <- "web" 
mtitle1 <- paste(kwd, stp, sep=", ")
gt <- gtrends(kwd, reg, tsd, stp, cct)
x1 <- gt$interest_over_time$hits
d1 <- gt$interest_over_time$date

kwd <- kwd1
stp <- "news" 
mtitle2 <- paste(kwd, stp, sep=", ")
gt <- gtrends(kwd, reg, tsd, stp, cct)
x2 <- gt$interest_over_time$hits
d2 <- gt$interest_over_time$date

kwd <- kwd2
stp <- "web" 
mtitle3 <- paste(kwd, stp, sep=", ")
gt <- gtrends(kwd, reg, tsd, stp, cct)
x3 <- gt$interest_over_time$hits
d3 <- gt$interest_over_time$date

kwd <- kwd2
stp <- "news" 
mtitle4 <- paste(kwd, stp, sep=", ")
gt <- gtrends(kwd, reg, tsd, stp, cct)
x4 <- gt$interest_over_time$hits
d4 <- gt$interest_over_time$date

sel <- d1 %in% dmr
d <- d1[sel]
z1 <- x1[sel]
z2 <- x2[sel]
z3 <- x3[sel]
z4 <- x4[sel]

pdf(file=paste("out/", su, "a.pdf", sep=""), width=11.7, height=8.3)
par(mfrow=c(2,2))
plot(d, z1, type="l", main=mtitle1, xlab="", ylab="")
plot(d, z2, type="l", main=mtitle2, xlab="", ylab="")
plot(d, z3, type="l", main=mtitle3, xlab="", ylab="")
plot(d, z4, type="l", main=mtitle4, xlab="", ylab="")
par(mfrow=c(1,1))
dev.off()

z <- cbind(z1, z2, z3, z4)

# Combination: simple average
s <- rowMeans(z, na.rm=TRUE)

# Compare uncleaned series
per1 <- d<as.Date("2012-01-01")
per2 <- d>=as.Date("2012-01-01")
c1 <- round(cor(s[per1], xmr[per1,su]),2)
c2 <- round(cor(s[per2], xmr[per2,su]),2)
c3 <- round(cor(s, xmr[,su]),2)

sub <- paste("Correlation: ", c1, "(<2012), ",
             c2, "(>2012), ", c3, "(Overall)", sep="")

pdf(file=paste("out/", su, "b.pdf", sep=""), width=11.7, height=8.3)
par(mar=c(5,4,4,5)+.1)
plot(d, s, type="l", xlab=sub, ylab="Google",
     main=mainkwd, col="blue")
par(new=TRUE)
plot(d, xmr[,su], type="l", xlab="", ylab="", xaxt="n",yaxt="n",
     col="red")
axis(side = 4)
mtext(side = 4, line = 3, "Reuters")
dev.off()

# Outliers?
pdf(file=paste("out/", su, "c.pdf", sep=""), width=8.3, height=11.7)
par(mfrow=c(2,1))

# od <- scores(s, type="z", prob=0.99)
od <- away.mean(s, 4)
op <- od
op[which(op==TRUE)] <- s[which(op==TRUE)]
op[op==FALSE] <- NA

plot(d, s, main=paste("Google ", mainkwd, sep=""), type="l",
     ylab="", xlab="")
points(d, op, col="red", pch=19)

# clean the series
sc <- s
sc[od] <- median(s)

#od <- scores(xmr[,su], type="z", prob=0.99)
od <- away.mean(xmr[,su], 4)
op <- od
op[which(op==TRUE)] <- xmr[which(op==TRUE),su]
op[op==FALSE] <- NA

plot(d, xmr[,su], main=paste("Reuters ", mainkwd, sep=""), type="l",
     ylab="", xlab="")
points(d, op, col="red", pch=19)

# clean the series
rc <- xmr[,su]
rc[od] <- median(rc)

par(mfrow=c(1,1))
dev.off()

# Seasonal?
tss <- ts(sc, frequency=12)
ss <- stl(tss, s.window="per")
pdf(file=paste("out/", su, "d1.pdf", sep=""), width=8.3, height=11.7)
plot(ss, main=paste("Google ", mainkwd, sep=""))
dev.off()
# Deseasonalise but do not de-trend
scc <- tss-ss$time.series[,1]

tss <- ts(rc, frequency=12)
ss <- stl(tss, s.window="per")
pdf(file=paste("out/", su, "d2.pdf", sep=""), width=8.3, height=11.7)
plot(ss, main=paste("Reuters ", mainkwd, sep=""))
dev.off()
# Deseasonalise but do not de-trend
rcc <- tss-ss$time.series[,1]

# Compare the cleaned series
per1 <- d<as.Date("2012-01-01")
per2 <- d>=as.Date("2012-01-01")
c1 <- round(cor(scc[per1], rcc[per1]),2)
c2 <- round(cor(scc[per2], rcc[per2]),2)
c3 <- round(cor(scc, rcc),2)

sub <- paste("Correlation: ", c1, "(<2012), ",
             c2, "(>2012), ", c3, "(Overall)", sep="")

pdf(file=paste("out/", su, "e.pdf", sep=""), width=11.7, height=8.3)
par(mar=c(5,4,4,5)+.1)
plot(d, scc, type="l", xlab=sub, ylab="Google",
     main=paste(mainkwd, ", clean", sep=""), col="blue")
par(new=TRUE)
plot(d, rcc, type="l", xlab="", ylab="", xaxt="n",yaxt="n",
     col="red")
axis(side = 4)
mtext(side = 4, line = 3, "Reuters")
dev.off()


ktemp <- cbind(as.numeric(sc), as.numeric(scc), as.numeric(rc), as.numeric(rcc))
rownames(ktemp) <- as.character(d)
colnames(ktemp) <- c(paste("Google-",mainkwd, "-Raw", sep=""),
                     paste("Google-",mainkwd, "-Cleaned", sep=""),
                     paste("Reuters-",mainkwd, "-Raw", sep=""),
                     paste("Reuters-",mainkwd, "-Cleaned", sep=""))
write.csv(ktemp, paste("out/OUTx-",mainkwd, ".csv",sep=""))
