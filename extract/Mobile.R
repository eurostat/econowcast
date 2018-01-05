rm(list=ls(all=TRUE))
setwd("../data/mobile/")

# Load the necessary data.
# Each file corresponds to a single day.
days <- c("sms-call-internet-mi-2013-11-01.csv",
          "sms-call-internet-mi-2013-11-02.csv",
          "sms-call-internet-mi-2013-11-03.csv",
          "sms-call-internet-mi-2013-11-04.csv",
          "sms-call-internet-mi-2013-11-05.csv",
          "sms-call-internet-mi-2013-11-06.csv",
          "sms-call-internet-mi-2013-11-07.csv")

# Create some labels to be used in the plots later		  
days.l <- c("2013-11-01, Friday", "2013-11-02, Saturday", "2013-11-03, Sunday",
            "2013-11-04, Monday", "2013-11-04, Tuesday", "2013-11-05, Wednesday",
            "2013-11-07, Thursday")
days.l2 <- c("2013-11-01", "2013-11-02", "2013-11-03",
            "2013-11-04", "2013-11-04", "2013-11-05",
            "2013-11-07")

# Create a sparse matrix to store the results	
result1 <- array(NA, dim=c(24,5,NROW(days)))

# We start with a loop.
# j runs for each different day. In the above we have 7 days (NROW(days)).
for(j in 1:NROW(days))
{
  # Load the data for day j.
  x <- read.csv(paste("raw/",days[j], sep=""), header=TRUE)

  # Identify unique users
  ud <- unique(x[,1])
  ui <- unique(x[,2])
  
  # Approach 1: Total activity during the day across users
  # Create a sparse matrix to store the results
  totact <- matrix(NA, NROW(ud), 5)
  rownames(totact) <- as.character(ud)
  colnames(totact) <- c("smsin","smsout","callin","callout","internet")
  
  for(i in 1:NROW(ud))
  {
    it <- ud[i]
    choose <- which(x[,1]==it)
 	# Input. x which has the data and here we are looping across users.
    totact[i,1] <- sum(x[it, 4], na.rm=TRUE)
    totact[i,2] <- sum(x[it, 5], na.rm=TRUE)
    totact[i,3] <- sum(x[it, 6], na.rm=TRUE)
    totact[i,4] <- sum(x[it, 7], na.rm=TRUE)
    totact[i,5] <- sum(x[it, 8], na.rm=TRUE)
  }
  result1[,,j] <- totact
  cat("day ", j, " just done - still left ", NROW(days), "\n")
}

# We are now ready to create some plots
# First, generate the colours.
cols <- rainbow(NROW(days), alpha = 1)

# Calls In and Out
# Using the 3rd and 4th column of array across days, "i", we can aggregate (sum) the calling activity.
i <- 1
plot(result1[,3,i]+result1[,4,i], type="l", xlab="Hours", ylab="Call", col=cols[i],
     main="Total Calls")

# Add the remaining days looping across "i"
for(i in 2:NROW(days))
{
  lines(result1[,3,i]+result1[,4,i], col=cols[i])
}
legend("topleft", legend=days.l, col=cols, lty=rep(1, NROW(days)),
       cex=0.6)

# Calls Internet
# Using the 5th column of array across days, "i", we can aggregate (sum) the internet activity.
i <- 1
plot(result1[,5,i], type="l", xlab="Hours", ylab="Call", col=cols[i],
     main="Total Internet Activity")
# Add the remaining days looping across "i"
for(i in 2:NROW(days))
{
  lines(result1[,5,i], col=cols[i])
}
legend("topleft", legend=days.l, col=cols, lty=rep(1, NROW(days)),
       cex=0.6)

# SMS
# Using the 1st and 2nd column of array across days, "i", we can aggregate (sum) the SMS activity.
i <- 1
plot(result1[,1,i]+result1[,2,i], type="l", xlab="Hours", ylab="SMS", col=cols[i],
     main="Total SMS")
# Add the remaining days looping across "i"
for(i in 2:NROW(days))
{
  lines(result1[,1,i]+result1[,2,i], col=cols[i])
}
legend("topleft", legend=days.l, col=cols, lty=rep(1, NROW(days)),
       cex=0.6)

# Total Calls across time
i <- 1
tc <- rep(NA, NROW(days)) 	# create a vector to store call activity
ti <- rep(NA, NROW(days))	# create a vector to store internet activity
tsms <- rep(NA, NROW(days))	# create a vector to store sms activity

# Now we are aggregating across time
for(i in 1:NROW(days))
{
  tc[i] <- sum(result1[,3,i]+result1[,4,i])
  ti[i] <- sum(result1[,5,i])
  tsms[i] <- sum(result1[,1,i]+result1[,2,i])
}

plot(as.Date(days.l2), tc, type="l", xlab="", ylab="Activity", main="Total Calls")
plot(as.Date(days.l2), ti, type="l", xlab="", ylab="Activity", main="Internet")
plot(as.Date(days.l2), tsms, type="l", xlab="", ylab="Activity", main="SMS")

write.csv(as.matrix(head(x)), "out/sample.csv")

                    