rm(list=ls(all=TRUE))
setwd("../data/twitter")
library(twitteR)
library(lubridate)

# insert your own keys
consumer_key <- "blahblahblah"
consumer_secret <- "blahblahblah"
access_token <- "blahblahblah"
access_secret <- "blahblahblah"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

rdmTweets <- searchTwitter('#gbpusd', n=6500)
df <- do.call("rbind", lapply(rdmTweets, as.data.frame))

write.csv(head(df), "./out/twitter-sample.csv")

# By user
counts <- table(df$screenName)
barplot(counts, main="GBPUSD by User")

# By Time
d <- df[,5]

# What are the days
days <- sort(unique(date(d)))

# What are the hours
hours <- sort(unique(hour(d)))

# Extract the number of tweets
tweets.ph <- matrix(NA, NROW(days), NROW(hours))
day.hour.keep <- NULL
for(i in 1:NROW(days))
{
  sel1 <- which(date(d)==days[i])
  x1 <- df[sel1,]
  xd1 <- x1[,5]
  
  for(j in 1:NROW(hours))
  {
    sel2 <- which(hour(xd1)==hours[j])
    tweets.ph[i,j] <- NROW(sel2)
    day.hour.keep <- c(day.hour.keep, paste(days[i], hours[j], sep=" "))
  }
}
rownames(tweets.ph) <- as.character(days)
colnames(tweets.ph) <- as.character(hours)

# hourly time series
hts <- as.vector(t(tweets.ph))

day.hour.keep <- ymd_h(day.hour.keep)
plot(day.hour.keep, hts, type="l", main="GBPUSD per Hour, Daily",
     xlab="", ylab="Number of Tweets")

# Specific hours across dates
plot(apply(tweets.ph, 2, mean), type="l",
     main="GBPUSD Hourly Tweets Across Days", xlab="Hour",
     ylab="Average Number of Tweets", lwd=2)

# Aggregate per day
plot(days, apply(tweets.ph, 1, sum), type="l",
     main="GBPUSD Daily Aggregate", xlab="Hour",
     ylab="Aggregate Number of Tweets", lwd=2)

