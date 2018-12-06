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

# Which weekly data is available?
zw <- as.matrix(xwF[1:which(dwF==cur.date),])
zG <- as.matrix(xG[1:which(dwF==cur.date),])
zR <- as.matrix(xR[1:which(dwF==cur.date),])

# Now let's carefully construct the monthly availability as we did for the target
zm <- matrix(NA, NROW(y), NCOL(xmF))
for(ii in 1:NCOL(xmF))
{
  ztemp <- as.matrix(xmF[1:(which(dmF==foredate)),ii])
  
  dor <- paste(min(year(foredatew)), "-",
               givemonth(min(month(foredatew))),"-", cmF[2,ii], sep="")
  dor <- as.Date(dor)
  if(cur.date<dor){
    if(cmF[1,ii]<0){
      ztemp[(NROW(ztemp)+cmF[1,ii]):NROW(ztemp),] <- NA
    }
  }else{
    if(cmF[1,ii]<0){
      ztemp[(NROW(ztemp)+cmF[1,ii]+1):NROW(ztemp),] <- NA
    }
  }
  zm[,ii] <- ztemp
}
colnames(zm) <- colnames(xmF)
colnames(zw) <- colnames(xwF)
rownames(zm) <- rownames(y)

# Now, we must transform the data to stationarity
YSAV <- shiftNAs(y)
ys <- transformdata(y, as.matrix(cT[,target])) # target
zws <- transformdata(zw, cwF)                  # weekly
zGs <- transformdata(zG, cG)                   # google
zRs <- transformdata(zR, as.matrix(cR))        # reuters
zms <- transformdata(zm, cmF)                  # monthly

# Now we should transform the weekly to monthly
# taking the average of what we have so far
vws <- weeklytomonthly(ys, zws)
vGs <- weeklytomonthly(ys, zGs)
vRs <- weeklytomonthly(ys, zRs)

# We have various NAs here and there
# There are wo ways to deal with that
# (i) extrapolation
# (ii) shifting and using all available information without caring about time alignment
# we will adopt the second in order to have less chances for model misspecifications
ys <- shiftNAs(ys)
vws <- shiftNAs(vws)
vGs <- shiftNAs(vGs)
vRs <- shiftNAs(vRs)
vms <- shiftNAs(zms)

# Make sure that there are no NAs in the top rows
nas <- NULL
nas <- c(nas, sum(is.na(ys)))
for(j in 1:NCOL(vws)){ nas <- c(nas, sum(is.na(vws[,j]))) }
for(j in 1:NCOL(vGs)){ nas <- c(nas, sum(is.na(vGs[,j]))) }
for(j in 1:NCOL(vRs)){ nas <- c(nas, sum(is.na(vRs[,j]))) }
for(j in 1:NCOL(vms)){ nas <- c(nas, sum(is.na(vms[,j]))) }
nas <- max(nas)



