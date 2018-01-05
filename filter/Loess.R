rm(list=ls(all=TRUE))
setwd("../data/outlier/")

library("mgcv")
library("lubridate")

x <-c(39,  73,  41,  76,  75,  47,   4,  53,  40,  47,  31,  33,
         58,  85,  61,  98,  90,  59,  34,  74,  78,  74,  56,  55,
         91, 125,  96, 135, 131, 103,  86, 116, 117, 128, 113, 123)

x <- ts(data=x, frequency=12, start=c(2015,1))
mts <- as.numeric(time(x))
tms <- date_decimal(mts)

mod <- gamm(temp ~ s(day.of.year, bs = "cc") + s(time, bs = "cr"),
            data = cairo2, method = "REML",
            correlation = corAR1(form = ~ 1 | year),
            knots = list(day.of.year = c(0, 366)))

time(x)


stl.out <- stl(x, s.window="per")
stl.x <- stl.out$time.series
plot(stl.out)

# Check how trend is calculated
tr <- lowess(x)

# check how seasonality is calculated when s.window="per

lines(tr, col="red")

plot(x, xlab="", ylab="", main="Loess Example")

which.cycle <- cycle(x)
cy <- tapply(x, which.cycle, mean)[which.cycle]

plot(cy, type="l")
