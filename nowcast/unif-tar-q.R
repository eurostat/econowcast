# Extract the target
y <- as.matrix(Y[1:(which(dmF==foredate)),])
dor <- paste(min(year(foredatew)), "-",
             givemonth(min(month(foredatew))),"-", cT[2,target], sep="")
dor <- as.Date(dor)
if(cur.date<dor){
  if(cT[1,target]<0){
    y[(NROW(y)+cT[1,target]):NROW(y),] <- NA
  }
}else{
  if(cT[1,target]<0){
    y[(NROW(y)+cT[1,target]+1):NROW(y),] <- NA
  }
}

# Now here we have quarterly
# extract the actual quarters
Ydold <- rownames(as.matrix(na.omit(as.matrix(Yold))))
Ydold <- as.Date(Ydold)
Ydold <- Ydold[Ydold<=max(as.Date(rownames(y)))]
Ydold <- as.character(Ydold)

y <- as.matrix(y[Ydold,])
ys <- shiftNAs(y)
Yin <- as.numeric(na.omit(ys))