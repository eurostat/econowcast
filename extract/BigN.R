rm(list=ls(all=TRUE))
setwd("../data/bd")

library(bigmemory)
library(biganalytics)
require(ggplot2)
library(tabplot)
options(bigmemory.allow.dimnames=TRUE)

# Let's try to generate a real big dataset
# This will be split in chunks, as is the case in real life
#
# In BD applications we have a lot of .txt files with the data
# usually the number of rows is really big and the number of columns is not
#
# For example, see Twitter
# The historical feed will be a lot of GB but the columns will only be about
# ten or twenty as described in Task 2

# Number of Monte Carlos
MC <- 1000

# Say we have 100,000 time periods and 1,000,000 events per period
tp <- 100000
nev <- 1000000

# We will split this in 10000s to make it easier
iS <- 10000
N <- tp/iS

e.par <- 1

i <- 1
# for(i in 1:MC)

data <- matrix(NA, iS, N)

ts <- Sys.time()
s <- 1
for(s in 1:iS)
{
  d <- big.matrix(nev, N, type="double",
                  dimnames=list(row=paste("nev", 1:nev, sep=""),
                                col=paste("N", 1:N, sep="")))
  e <- big.matrix(nev, N, type="double",
                  dimnames=list(row=paste("nev", 1:nev, sep=""),
                                col=paste("N", 1:N, sep="")))
  
  for(j in 1:NCOL(d))
  {
    d[,j] <- round(runif(nev, min=0, max=1))
    e[,j] <- runif(nev, min=0, max=e.par)
  }
  
  data[s, ] <- apply(e, 2, sum)
}
te <- Sys.time()




