rm(list=ls(all=TRUE))
setwd("../data/outlier/")
library("lubridate")
library("outliers")
library("moments")
library("seasonal")
library("forecast")
library("ForeCA")

# Function to calculate desc. stats.
desc.stat <- function(z)
{
  cc <- c(mean(z), median(z), sd(z), min(z),max(z), skewness(z), kurtosis(z))
  cc <- as.matrix(cc)
  rownames(cc) <- c("Mean", "Median", "Std. Dev.", "Min", "Max", "Skewness", "Kurtosis")
  return(cc)
}

# Set the seed
set.seed(123456)

# Starting date
start <- as.POSIXct("2017-01-01")
interval <- 1  # observation every minute
ndays <- 500
end <- start + as.difftime(ndays, units="days")  # say for 500 days
d <- seq(from=start, by=interval*60, to=end)

# Generate a dataset
# which takes values 0:4
# this could also be a categorical dataset translated to numerical
x <- round(runif(NROW(d), min=0, max=4))

# Add some outliers
o <- 2*round(NROW(x)^0.5)
o <- round(runif(o, min=1, max=NROW(x)))
x[o] <- mean(x)+6*sd(x)

# Now, show a day
ysel <- which(date(d)==as.Date("2017-01-01"))
y <- x[ysel]
yd <- d[ysel]
plot(yd, y, type="l", xlab="Time", ylab="", main="2017-01-01")

od1 <- scores(y, type="z", prob=0.90)
od2 <- scores(y, type="z", prob=0.95)
od3 <- scores(y, type="z", prob=0.99)

op <- od1
op[which(op==TRUE)] <- y[which(op==TRUE)]
op[op==FALSE] <- NA
points(yd, op, col="green", pch=19)

op <- od2
op[which(op==TRUE)] <- y[which(op==TRUE)]
op[op==FALSE] <- NA
points(yd, op, col="blue", pch=19)

op <- od3
op[which(op==TRUE)] <- y[which(op==TRUE)]
op[op==FALSE] <- NA
points(yd, op, col="red", pch=19)

boxplot(y, main="2017-01-01")

# Remove outliers on a daily basis and assign the day median
# Then, aggregate daily
aggd <- unique(date(d))
aggx <- rep(NA, NROW(aggd))
aggx2 <- aggx
aggdl <- list()
for(i in 1:NROW(aggd))
{
  xsel <- which(date(d)==aggd[i])
  od <- scores(x[xsel], type="z", prob=0.99)
  
  x2 <- x[xsel]
  x2[which(od==TRUE)] <- median(x2)
  
  aggx[i] <- sum(x[xsel])
  aggx2[i] <- sum(x2)
  
  aggdl <- c(aggdl, list(x[xsel]))
  cat("Now done ", i, " of ", NROW(aggd), "\n")
}

aggd <- aggd[1:ndays]
aggx <- aggx[1:ndays]
aggx2 <- aggx2[1:ndays]
aggdl <- aggdl[,,1:ndays]

# Do some daily box-plots
aggdl <- matrix(unlist(aggdl), ncol=ndays, byrow=TRUE)
bp <- boxplot(aggdl[,1:50], main="First 50 days")

# Compare with and without
plot(aggd, aggx, type="l", main="Daily Aggregation", xlab="", ylab="")
lines(aggd, aggx2, col="blue")

dstat <- round(cbind(desc.stat(aggx), desc.stat(aggx2)),3)
write.csv(dstat, "./out/dstat.csv")

boxplot(aggx2, main="Daily Time Series, N=500")

od4 <- scores(aggx2, type="z", prob=0.99)
plot(aggd, aggx2, type="l", xlab="Time", ylab="", main="Daily Time Series, N=500")

op <- od4
op[which(op==TRUE)] <- aggx2[which(op==TRUE)]
op[op==FALSE] <- NA
points(aggd, op, col="red", pch=19)

# Now let's try to introduce seasonal effects which will lead
# to seasonal daily time series
# we re-generate the series without the outliers
# using the set.seed() this does not cause any problems
x <- round(runif(NROW(d), min=0, max=4))*(1+wday(d)/100)
aggd <- unique(date(d))
aggx <- rep(NA, NROW(aggd))
aggx2 <- aggx
aggdl <- list()
for(i in 1:NROW(aggd))
{
  xsel <- which(date(d)==aggd[i])
  od <- scores(xs[xsel], type="z", prob=0.99)
  
  x2 <- x[xsel]
  x2[which(od==TRUE)] <- median(x2)
  
  aggx[i] <- sum(x[xsel])
  aggx2[i] <- sum(x2)
  
  aggdl <- c(aggdl, list(x[xsel]))
  cat("Now done ", i, " of ", NROW(aggd), "\n")
}

aggd <- aggd[1:ndays]
aggx <- aggx[1:ndays]
aggx2 <- aggx2[1:ndays]
aggdl <- aggdl[,,1:ndays]

# Compare
plot(aggd, aggx, type="l",
     main="Daily Aggregation, Weekly Pattern", xlab="", ylab="")

par(mfrow=c(1,2))
xsel <- which(date(d)=="2017-01-01")
plot(d[xsel], x[xsel], type="l", main="2017-01-01", xlab="", ylab="")

xsel <- which(date(d)=="2017-01-07")
plot(d[xsel], x[xsel], type="l", main="2017-01-07", xlab="", ylab="")
par(mfrow=c(1,1))

plot(d[1:(14*1440)], x[1:(14*1440)], type="l",
     main="Daily Aggregation, Weekly Pattern", xlab="", ylab="")

# Decompose
# Input: aggx is a numeric vector of aggregated time series in weekly frequency
# 		First, we transform the numeric vector in a time series (ts) object
#			correctly specifying the frequency.
tsaggx <- ts(aggx, frequency=7)

# Then, we use the ts object, tsaggx, as the input in the LOESS function.
# 	s.window: can be a string "periodic" or "per" which reads the frequency from the ts transformation
#				otherwise it can be a user choice.
ss <- stl(tsaggx, s.window="per")

# Plot the output
plot(ss, main="Daily Aggregation, Weekly Pattern")

# Extract the seasonal component (xs) and the trend component (xt)
xs <- ss$time.series[,1]
xt <- ss$time.series[,2]

# Calculate the cleaned series
xc <- tsaggx-xs-xt

# And create a plot
# plot(xc, type="l", xlab="", ylab="", main="Detrended and Deseasonalised")
plot(aggd, xc, type="l", xlab="", ylab="", main="Detrended and Deseasonalised")

od5 <- scores(xc, type="z", prob=0.99)
op <- od5
op[which(op==TRUE)] <- xc[which(op==TRUE)]
op[op==FALSE] <- NA
points(aggd, op, col="red", pch=19)

