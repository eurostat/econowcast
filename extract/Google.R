rm(list=ls(all=TRUE))
setwd("../data/google/")
# library(devtools)
# devtools::install_github("PMassicotte/gtrendsR")
library("gtrendsR")

# Simple Example
####################################################################
# User Input
kwd <- "gbp"         # Keyword(s)
reg <- "GB"             # Region
tsd <- "all"            # Time Frame: "all" (since 2004),
                        # "today+XXX-y" (last XXX years)
stp <- "web"            # "web", "news", "images", "froogle", "youtube"
cct <- 0                # category

gt <- gtrends(kwd, reg, tsd, stp, cct)
plot(gt)

mkwd <- gt$interest_over_time$hits
mdd <- gt$interest_over_time$date


# Use Google Correlates
####################################################################
# Load the .csv downloaded from Google Correlates
gcor <- read.csv("raw/correlate-gbp.csv", header=FALSE)
gcor <- as.matrix(gcor)

kwds <- gcor[1,2:NCOL(gcor)]

# remove the main keyword from the list
kwds <- kwds[which(kwds!=kwd)]

# Instead of using all NCOL(gcor) use just the first 10
# for demo purposed
kwds <- kwds[1:10]

gckwds <- matrix(NA, NROW(mkwd), NROW(kwds))

for(i in 1:NROW(kwds))
{
  ikwd <- kwds[i]
  gt <- gtrends(ikwd, reg, tsd, stp, cct)
  gckwds[,i] <- gt$interest_over_time$hits
  cat("Downloaded ", i, " of remaining", NROW(kwds), " Google Trends", "\n")
  Sys.sleep(10)
}

# Now, we will plot the main kwd and 9 of the above
# most correlated ones
par(mfrow=c(5,2))
plot(mdd, mkwd, main=kwd, xlab="", ylab="% Searches", type="l")
for(i in 1:9)
{
  plot(mdd, gckwds[,i], main=kwds[i], xlab="", ylab="% Searches", type="l")
}
par(mfrow=c(1,1))


