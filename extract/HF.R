rm(list=ls(all=TRUE))
setwd("../data/hf/")
library("highfrequency")

data("raw/sample_tdataraw")
data("raw/sample_qdataraw")
data("raw/sample_tdata")
data("raw/sample_qdata")

write.csv(as.matrix(head(sample_tdataraw)), "out/tdataraw.csv")
write.csv(as.matrix(head(sample_qdataraw)), "out/qdataraw.csv")

write.csv(as.matrix(head(sample_tdata)), "out/tdata.csv")
write.csv(as.matrix(head(sample_qdata)), "out/qdata.csv")

#aggregate price data to the 5 mins frequency
tp <- aggregatePrice(sample_tdata$PRICE,on="mins",k=5)
tq <- aggregateQuotes(sample_qdata,on="mins",k=5)

RVp <- sum((makeReturns(tp))^2)

data(sample_returns_5min)
r <- sample_returns_5min

RV <- rowSums(r^2)

plot(RV, type="l", main="EUR/USD 5-min RV", xlab="Time", ylab="RV")

