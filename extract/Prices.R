rm(list=ls(all=TRUE))
setwd("../data/prices/")

# Load the data
x <- read.csv("arg-sample2.csv", header=TRUE)
x <- as.matrix(x)
x <- x[,c(5, 1, 7, 6)] # Extract the necessary columns
colnames(x) <- c("date","id", "cat", "price")

d <- as.Date(x[,1])		# create the dates sequence
ud <- sort(unique(d))	# extract unique dates for time series aggregation
uid <- unique(x[,2])	# extract unique id's
uc <- unique(x[,3])		# extract unique categories

# Create sparse matrix to store the results
cats <- matrix(NA, NROW(ud)-1, NROW(uc))
colnames(cats) <- uc
rownames(cats) <- as.character(ud[2:NROW(ud)])

# Start the loop across categories
for(i in 1:NROW(uc))
{
  xcat <- which(x[,3]==uc[i])
  xcat <- x[xcat,]
  
  tmat <- matrix(NA, NROW(ud), NROW(uid))
  colnames(tmat) <- c(uid)
  
  # Create a doouble loop: (j) across id's and (jj) across dates
  for(j in 1:NROW(uid))
  {
    xcat2 <- which(xcat[,2]==uid[j])
    xcat2 <- xcat[xcat2,]
    
    for(jj in 1:NROW(ud))
    {
      cw <- which(xcat2[,1]==ud[jj])
      
      if(NROW(cw)==0){
        tmat[jj,j] <- NA
      }else{
        tmat[jj,j] <- xcat2[cw,4]
      }
    }
  }
  # ensure that the result is numeric
  tmat <- apply(tmat, 2, as.numeric)
  
  R <- tmat[2:NROW(tmat),]/tmat[1:(NROW(tmat)-1),]
  R <- apply(R, 1, prod, na.rm=TRUE)
  R <- R^(1/NCOL(tmat))
  
  cats[,i] <- R
}

# Cumulate across time
I <- apply(cats, 2, cumprod)
w <- as.matrix(rep(1/NCOL(I), NCOL(I)))

# use the approprite weights as in Cavallo
S <- I %*% w

# Produce the final CPI estimate plot.
plot(as.Date(rownames(S)), S, type="l", main="Online Prices CPI", xlab="Time", ylab="Index")

