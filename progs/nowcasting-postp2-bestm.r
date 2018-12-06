rm(list=ls(all=TRUE))
library("Matrix")
library("abind")
library("rugarch")
library("forecast")
setwd("output/")

# MAINOUT2-target.ei_isin_m.COUNTRYCO        MONTHLY  --  Industrial Production
# MAINOUT2-target.ei_lmhr_m.COUNTRYCO        MONTHLY  --  Unemployment Rate
# MAINOUT2-target.namq_10_a10_e.COUNTRYCO    QUARTERLY -  Employment in Thousand of Persons
# MAINOUT2-target.prc_hicp_midx.COUNTRYCO    MONTHLY  --  Harmonised CPI
# MAINOUT2-target.namq_10_gdp.COUNTRYCO      QUARTERLY -  GDP

# Industrial-Production
# Unemployment-Rate
# EmploymentTHP
# HCPI
# GDP

# 2) we will create the best strategy models
# 3) we will create model averaging
# and then create the best model strategy as well as the averaging
xfile <- "MAINOUT2-target.namq_10_gdp.UK"
saveindir <- "output/UK/"
saveindir2 <- "output/UK/"
xsername <- "GDP"

load(paste("output/", xfile, ".Rdata",sep=""))

# Bind all methods and univ models
all5w <- abind(for5w, uni5w, along=3)
all4w <- abind(for4w, uni4w, along=3)
all3w <- abind(for3w, uni3w, along=3)
all2w <- abind(for2w, uni2w, along=3)
all1w <- abind(for1w, uni1w, along=3)
all0w <- abind(for0w, uni0w, along=3)

# Extract the point forecasts to create the errors
# Extract all the point forecasts
arow <- dim(all5w)[1]
acol <- dim(all5w)[3]
anams <- unlist(dimnames(all5w)[3])
for5p <- matrix(NA, arow, acol)
rownames(for5p) <- rownames(Y)
colnames(for5p) <- anams
for4p <- for5p
for3p <- for5p
for2p <- for5p
for1p <- for5p
for0p <- for5p
for(j in 1:acol)
{
  for5p[,anams[j]] <- all5w[,1,anams[j]]
  for4p[,anams[j]] <- all4w[,1,anams[j]]
  for3p[,anams[j]] <- all3w[,1,anams[j]]
  for2p[,anams[j]] <- all2w[,1,anams[j]]
  for1p[,anams[j]] <- all1w[,1,anams[j]]
  for0p[,anams[j]] <- all0w[,1,anams[j]]
}

for5p <- na.omit(for5p)
for4p <- na.omit(for4p)
for3p <- na.omit(for3p)
for2p <- na.omit(for2p)
for1p <- na.omit(for1p)
for0p <- na.omit(for0p)

W <- as.matrix(Y[rownames(for5p),])
Wd <- as.Date(rownames(W))

# Create the forecast error
err5p <- matrix(as.numeric(W), NROW(W), NROW(anams))-for5p
err4p <- matrix(as.numeric(W), NROW(W), NROW(anams))-for4p
err3p <- matrix(as.numeric(W), NROW(W), NROW(anams))-for3p
err2p <- matrix(as.numeric(W), NROW(W), NROW(anams))-for2p
err1p <- matrix(as.numeric(W), NROW(W), NROW(anams))-for1p
err0p <- matrix(as.numeric(W), NROW(W), NROW(anams))-for0p

# create the best models using the cumulative error since the
# out-of-sample is really small.
# each best model will be different for each release
# because we might have something good for 5 weeks ago but not so good for 2 weeks
bnams <- c("Best1", "Best3", "Best5", "Best10")
best5w <- array(NA, c(NROW(Y), NROW(pintv)+1, NROW(bnams)),
               dimnames=list(rownames(Y), c("PointF", as.character(pintv)), bnams))
best4w <- best5w
best3w <- best5w
best2w <- best5w
best1w <- best5w
best0w <- best5w

bsel5w <- matrix(NA, NROW(W), 10)
rownames(bsel5w) <- as.character(Wd)
colnames(bsel5w) <- paste("best", 1:10, sep="")
bsel4w <- bsel3w <- bsel2w <- bsel1w <- bsel0w <- bsel5w

for(i in 1:NROW(Wd))
{
  savdat <- as.character(Wd[i])
  if(i==1){
    # start with the naive for all cases
    bmodel <- "Naive"
    best5w[savdat,,"Best1"]  <- all5w[savdat,,bmodel]
    best5w[savdat,,"Best3"]  <- all5w[savdat,,bmodel]
    best5w[savdat,,"Best5"]  <- all5w[savdat,,bmodel]
    best5w[savdat,,"Best10"] <- all5w[savdat,,bmodel]
    bsel5w[savdat,] <- rep(bmodel, 10)
    
    best4w[savdat,,"Best1"]  <- all4w[savdat,,bmodel]
    best4w[savdat,,"Best3"]  <- all4w[savdat,,bmodel]
    best4w[savdat,,"Best5"]  <- all4w[savdat,,bmodel]
    best4w[savdat,,"Best10"] <- all4w[savdat,,bmodel]
    bsel4w[savdat,] <- rep(bmodel, 10)
    
    best3w[savdat,,"Best1"]  <- all3w[savdat,,bmodel]
    best3w[savdat,,"Best3"]  <- all3w[savdat,,bmodel]
    best3w[savdat,,"Best5"]  <- all3w[savdat,,bmodel]
    best3w[savdat,,"Best10"] <- all3w[savdat,,bmodel]
    bsel3w[savdat,] <- rep(bmodel, 10)
    
    best2w[savdat,,"Best1"]  <- all2w[savdat,,bmodel]
    best2w[savdat,,"Best3"]  <- all2w[savdat,,bmodel]
    best2w[savdat,,"Best5"]  <- all2w[savdat,,bmodel]
    best2w[savdat,,"Best10"] <- all2w[savdat,,bmodel]
    bsel2w[savdat,] <- rep(bmodel, 10)
    
    best1w[savdat,,"Best1"]  <- all1w[savdat,,bmodel]
    best1w[savdat,,"Best3"]  <- all1w[savdat,,bmodel]
    best1w[savdat,,"Best5"]  <- all1w[savdat,,bmodel]
    best1w[savdat,,"Best10"] <- all1w[savdat,,bmodel]
    bsel1w[savdat,] <- rep(bmodel, 10)
    
    best0w[savdat,,"Best1"]  <- all0w[savdat,,bmodel]
    best0w[savdat,,"Best3"]  <- all0w[savdat,,bmodel]
    best0w[savdat,,"Best5"]  <- all0w[savdat,,bmodel]
    best0w[savdat,,"Best10"] <- all0w[savdat,,bmodel]
    bsel0w[savdat,] <- rep(bmodel, 10)
  }else{
    if(i==2){
      bmodel <- anams[order(sqrt(err5p[1:(i-1),]^2))[1:10]]
    }else{
      bmodel <- anams[order(sqrt(colMeans(err5p[1:(i-1),]^2)))[1:10]]
    }
    best5w[savdat,,"Best1"]   <- all5w[savdat,,bmodel[1]]
    best5w[savdat,,"Best3"]   <- rowMeans(all5w[savdat,,bmodel[1:3]])
    best5w[savdat,,"Best5"]   <- rowMeans(all5w[savdat,,bmodel[1:5]])
    best5w[savdat,,"Best10"]  <- rowMeans(all5w[savdat,,bmodel[1:10]])
    bsel5w[savdat,] <- bmodel
    
    if(i==2){
      bmodel <- anams[order(sqrt(err4p[1:(i-1),]^2))[1:10]]
    }else{
      bmodel <- anams[order(sqrt(colMeans(err4p[1:(i-1),]^2)))[1:10]]
    }
    best4w[savdat,,"Best1"]   <- all4w[savdat,,bmodel[1]]
    best4w[savdat,,"Best3"]   <- rowMeans(all4w[savdat,,bmodel[1:3]])
    best4w[savdat,,"Best5"]   <- rowMeans(all4w[savdat,,bmodel[1:5]])
    best4w[savdat,,"Best10"]  <- rowMeans(all4w[savdat,,bmodel[1:10]])
    bsel4w[savdat,] <- bmodel
    
    if(i==2){
      bmodel <- anams[order(sqrt(err3p[1:(i-1),]^2))[1:10]]
    }else{
      bmodel <- anams[order(sqrt(colMeans(err3p[1:(i-1),]^2)))[1:10]]
    }
    best3w[savdat,,"Best1"]   <- all3w[savdat,,bmodel[1]]
    best3w[savdat,,"Best3"]   <- rowMeans(all3w[savdat,,bmodel[1:3]])
    best3w[savdat,,"Best5"]   <- rowMeans(all3w[savdat,,bmodel[1:5]])
    best3w[savdat,,"Best10"]  <- rowMeans(all3w[savdat,,bmodel[1:10]])
    bsel3w[savdat,] <- bmodel
    
    if(i==2){
      bmodel <- anams[order(sqrt(err2p[1:(i-1),]^2))[1:10]]
    }else{
      bmodel <- anams[order(sqrt(colMeans(err2p[1:(i-1),]^2)))[1:10]]
    }
    best2w[savdat,,"Best1"]   <- all2w[savdat,,bmodel[1]]
    best2w[savdat,,"Best3"]   <- rowMeans(all2w[savdat,,bmodel[1:3]])
    best2w[savdat,,"Best5"]   <- rowMeans(all2w[savdat,,bmodel[1:5]])
    best2w[savdat,,"Best10"]  <- rowMeans(all2w[savdat,,bmodel[1:10]])
    bsel2w[savdat,] <- bmodel
    
    if(i==2){
      bmodel <- anams[order(sqrt(err1p[1:(i-1),]^2))[1:10]]
    }else{
      bmodel <- anams[order(sqrt(colMeans(err1p[1:(i-1),]^2)))[1:10]]
    }
    best1w[savdat,,"Best1"]   <- all1w[savdat,,bmodel[1]]
    best1w[savdat,,"Best3"]   <- rowMeans(all1w[savdat,,bmodel[1:3]])
    best1w[savdat,,"Best5"]   <- rowMeans(all1w[savdat,,bmodel[1:5]])
    best1w[savdat,,"Best10"]  <- rowMeans(all1w[savdat,,bmodel[1:10]])
    bsel1w[savdat,] <- bmodel
    
    if(i==2){
      bmodel <- anams[order(sqrt(err0p[1:(i-1),]^2))[1:10]]
    }else{
      bmodel <- anams[order(sqrt(colMeans(err0p[1:(i-1),]^2)))[1:10]]
    }
    best0w[savdat,,"Best1"]   <- all0w[savdat,,bmodel[1]]
    best0w[savdat,,"Best3"]   <- rowMeans(all0w[savdat,,bmodel[1:3]])
    best0w[savdat,,"Best5"]   <- rowMeans(all0w[savdat,,bmodel[1:5]])
    best0w[savdat,,"Best10"]  <- rowMeans(all0w[savdat,,bmodel[1:10]])
    bsel0w[savdat,] <- bmodel
  }
}

# now we have everything we want
# so let's try to produce statistics and figures
# Bind all methods and univ models
all5f <- abind(all5w, best5w, along=3)
all4f <- abind(all4w, best4w, along=3)
all3f <- abind(all3w, best3w, along=3)
all2f <- abind(all2w, best2w, along=3)
all1f <- abind(all1w, best1w, along=3)
all0f <- abind(all0w, best0w, along=3)

# Extract the point forecasts to create the errors
# Extract all the point forecasts
arow <- dim(all5f)[1]
acol <- dim(all5f)[3]
anams <- unlist(dimnames(all5f)[3])
for5p <- matrix(NA, arow, acol)
rownames(for5p) <- rownames(Y)
colnames(for5p) <- anams
for4p <- for5p
for3p <- for5p
for2p <- for5p
for1p <- for5p
for0p <- for5p
for(j in 1:acol)
{
  for5p[,anams[j]] <- all5f[,1,anams[j]]
  for4p[,anams[j]] <- all4f[,1,anams[j]]
  for3p[,anams[j]] <- all3f[,1,anams[j]]
  for2p[,anams[j]] <- all2f[,1,anams[j]]
  for1p[,anams[j]] <- all1f[,1,anams[j]]
  for0p[,anams[j]] <- all0f[,1,anams[j]]
}

for5p <- na.omit(for5p)
for4p <- na.omit(for4p)
for3p <- na.omit(for3p)
for2p <- na.omit(for2p)
for1p <- na.omit(for1p)
for0p <- na.omit(for0p)

W <- as.matrix(Y[rownames(for5p),])
Wd <- as.Date(rownames(W))

# Create the forecast error
err5p <- matrix(as.numeric(W), NROW(W), NROW(anams))-for5p
err4p <- matrix(as.numeric(W), NROW(W), NROW(anams))-for4p
err3p <- matrix(as.numeric(W), NROW(W), NROW(anams))-for3p
err2p <- matrix(as.numeric(W), NROW(W), NROW(anams))-for2p
err1p <- matrix(as.numeric(W), NROW(W), NROW(anams))-for1p
err0p <- matrix(as.numeric(W), NROW(W), NROW(anams))-for0p

benchmark <- "AR(1)"
# Create RMSFE
rmsfe5 <- as.matrix(sqrt(colMeans(err5p^2)))
rmsfe4 <- as.matrix(sqrt(colMeans(err4p^2)))
rmsfe3 <- as.matrix(sqrt(colMeans(err3p^2)))
rmsfe2 <- as.matrix(sqrt(colMeans(err2p^2)))
rmsfe1 <- as.matrix(sqrt(colMeans(err1p^2)))
rmsfe0 <- as.matrix(sqrt(colMeans(err0p^2)))
rmsfe <- cbind(rmsfe5, rmsfe4, rmsfe3, rmsfe2, rmsfe1, rmsfe0)

rmsfe5 <- rmsfe5/rmsfe5[benchmark,1]
rmsfe4 <- rmsfe4/rmsfe4[benchmark,1]
rmsfe3 <- rmsfe3/rmsfe3[benchmark,1]
rmsfe2 <- rmsfe2/rmsfe2[benchmark,1]
rmsfe1 <- rmsfe1/rmsfe1[benchmark,1]
rmsfe0 <- rmsfe0/rmsfe0[benchmark,1]
rmsfe.rel <- cbind(rmsfe5, rmsfe4, rmsfe3, rmsfe2, rmsfe1, rmsfe0)

# Create MAE
mae5 <- as.matrix(colMeans(abs(err5p)))
mae4 <- as.matrix(colMeans(abs(err4p)))
mae3 <- as.matrix(colMeans(abs(err3p)))
mae2 <- as.matrix(colMeans(abs(err2p)))
mae1 <- as.matrix(colMeans(abs(err1p)))
mae0 <- as.matrix(colMeans(abs(err0p)))
mae <- cbind(mae5, mae4, mae3, mae2, mae1, mae0)

mae5 <- mae5/mae5[benchmark,1]
mae4 <- mae4/mae4[benchmark,1]
mae3 <- mae3/mae3[benchmark,1]
mae2 <- mae2/mae2[benchmark,1]
mae1 <- mae1/mae1[benchmark,1]
mae0 <- mae0/mae0[benchmark,1]
mae.rel <- cbind(mae5, mae4, mae3, mae2, mae1, mae0)

# create the two sided Diebold Mariano
dm <- matrix(NA, NROW(mae), 6)

for(i in 1:NCOL(err5p))
{
  dmp <- NULL
  try( dmp <- dm.test(err5p[,i], err5p[,benchmark], alternative=c("two.sided"), h=1, power=2), silent=TRUE)
  if(is.null(dmp)==TRUE){ dmp <- NA }else{ dmp <- as.numeric(dmp$p.value) }
  dm[i,1] <- dmp
  
  dmp <- NULL
  try( dmp <- dm.test(err4p[,i], err4p[,benchmark], alternative=c("two.sided"), h=1, power=2), silent=TRUE)
  if(is.null(dmp)==TRUE){ dmp <- NA }else{ dmp <- as.numeric(dmp$p.value) }
  dm[i,2] <- dmp
  
  dmp <- NULL
  try( dmp <- dm.test(err3p[,i], err3p[,benchmark], alternative=c("two.sided"), h=1, power=2), silent=TRUE)
  if(is.null(dmp)==TRUE){ dmp <- NA }else{ dmp <- as.numeric(dmp$p.value) }
  dm[i,3] <- dmp
  
  dmp <- NULL
  try( dmp <- dm.test(err2p[,i], err2p[,benchmark], alternative=c("two.sided"), h=1, power=2), silent=TRUE)
  if(is.null(dmp)==TRUE){ dmp <- NA }else{ dmp <- as.numeric(dmp$p.value) }
  dm[i,4] <- dmp
  
  dmp <- NULL
  try( dmp <- dm.test(err1p[,i], err1p[,benchmark], alternative=c("two.sided"), h=1, power=2), silent=TRUE)
  if(is.null(dmp)==TRUE){ dmp <- NA }else{ dmp <- as.numeric(dmp$p.value) }
  dm[i,5] <- dmp
  
  dmp <- NULL
  try( dmp <- dm.test(err0p[,i], err0p[,benchmark], alternative=c("two.sided"), h=1, power=2), silent=TRUE)
  if(is.null(dmp)==TRUE){ dmp <- NA }else{ dmp <- as.numeric(dmp$p.value) }
  dm[i,6] <- dmp
}

# Calculate sign success ratio / directional measure
prevdat <- which(Yd==Wd[1])-1
WW <- rbind(Y[prevdat,], W)

WWc <- WW[2:NROW(WW)]-WW[1:(NROW(WW)-1)]
for5c <- for5p-WW[1:(NROW(WW)-1)]
for4c <- for4p-WW[1:(NROW(WW)-1)]
for3c <- for3p-WW[1:(NROW(WW)-1)]
for2c <- for2p-WW[1:(NROW(WW)-1)]
for1c <- for1p-WW[1:(NROW(WW)-1)]
for0c <- for0p-WW[1:(NROW(WW)-1)]

ssr <- matrix(NA, NROW(mae), 6)

sgnt <- sign(WWc)
for(i in 1:NCOL(for5c))
{
  ssr[i,1] <- sum(sgnt==sign(for5c[,i]))
  ssr[i,2] <- sum(sgnt==sign(for4c[,i]))
  ssr[i,3] <- sum(sgnt==sign(for3c[,i]))
  ssr[i,4] <- sum(sgnt==sign(for2c[,i]))
  ssr[i,5] <- sum(sgnt==sign(for1c[,i]))
  ssr[i,6] <- sum(sgnt==sign(for0c[,i]))
}
ssr <- ssr/NROW(for5c)
ssr <- ssr*100

dm <- round(dm, 2)
ssr <- round(ssr, 2)
colnames(ssr) <- colnames(mae) <- colnames(dm) <- colnames(rmsfe) <- c("5w","4w","3w", "2w", "1w", "0w")
rownames(ssr) <- rownames(mae)
rownames(dm) <- rownames(mae)

# Now, find the best 6 models in terms of MAE, RMSFE, SSR
# for 5w, 4w, 3w, 2w, 1w, 0w
# and make pointf plots
brmsfe5w <- rownames(rmsfe)[order(rmsfe[,1])[1:6]]
brmsfe4w <- rownames(rmsfe)[order(rmsfe[,2])[1:6]]
brmsfe3w <- rownames(rmsfe)[order(rmsfe[,3])[1:6]]
brmsfe2w <- rownames(rmsfe)[order(rmsfe[,4])[1:6]]
brmsfe1w <- rownames(rmsfe)[order(rmsfe[,5])[1:6]]
brmsfe0w <- rownames(rmsfe)[order(rmsfe[,6])[1:6]]

bmae5w <- rownames(mae)[order(mae[,1])[1:6]]
bmae4w <- rownames(mae)[order(mae[,2])[1:6]]
bmae3w <- rownames(mae)[order(mae[,3])[1:6]]
bmae2w <- rownames(mae)[order(mae[,4])[1:6]]
bmae1w <- rownames(mae)[order(mae[,5])[1:6]]
bmae0w <- rownames(mae)[order(mae[,6])[1:6]]

ssr5w <- rownames(ssr)[order(ssr[,1], decreasing=TRUE)[1:6]]
ssr4w <- rownames(ssr)[order(ssr[,2], decreasing=TRUE)[1:6]]
ssr3w <- rownames(ssr)[order(ssr[,3], decreasing=TRUE)[1:6]]
ssr2w <- rownames(ssr)[order(ssr[,4], decreasing=TRUE)[1:6]]
ssr1w <- rownames(ssr)[order(ssr[,5], decreasing=TRUE)[1:6]]
ssr0w <- rownames(ssr)[order(ssr[,6], decreasing=TRUE)[1:6]]

bestrmsfe <- cbind(brmsfe5w, brmsfe4w, brmsfe3w, brmsfe2w, brmsfe1w, brmsfe0w)
bestmae <- cbind(bmae5w, bmae4w, bmae3w, bmae2w, bmae1w, bmae0w)
bestssr <- cbind(ssr5w, ssr4w, ssr3w, ssr2w, ssr1w, ssr0w)

setwd(saveindir)
write.csv(rmsfe[order(rownames(rmsfe)),], paste(xsername, "-rmsfe.csv", sep=""))
write.csv(mae[order(rownames(mae)),], paste(xsername, "-mae.csv", sep=""))
write.csv(dm[order(rownames(dm)),], paste(xsername, "-dm.csv", sep=""))
write.csv(bestrmsfe, paste(xsername, "-bestrmsfe.csv", sep=""))
write.csv(bestmae, paste(xsername, "-bestmae.csv", sep=""))
write.csv(rmsfe.rel[order(rownames(rmsfe)),], paste(xsername, "-rmsfeREL.csv", sep=""))
write.csv(mae.rel[order(rownames(mae)),], paste(xsername, "-maeREL.csv", sep=""))

prevdat <- which(Yd==Wd[1])-1
WW <- rbind(Y[prevdat,], W)
rownames(WW) <- c(as.character(Yd[prevdat]), rownames(W))
Wd <- as.Date(rownames(WW))

xcols <- colorRampPalette(c("#FF0000", "#FFCCCC"))
xcols <- xcols(5)
xcols <- rev(xcols)
# xcols <- c("#FF6666", "#CC0000", "#00CC00", "#009900", "#99CCFF", "#0066CC")

for(j in 1:NROW(anams))
{
  jnam <- anams[j]
  fnam <- paste("zfig-", xsername,"-", jnam, ".pdf", sep="")
  
  # xdata <- cbind(for5p[,jnam], for4p[,jnam], for3p[,jnam],
  #                for2p[,jnam], for1p[,jnam], for0p[,jnam])
  
  xdata <- cbind(for5p[,jnam], for4p[,jnam], for3p[,jnam],
                 for2p[,jnam], for1p[,jnam])
  
  xdata <- rbind(matrix(NA, 1, NCOL(xdata)), xdata)
  rownames(xdata) <- rownames(WW)
  colnames(xdata) <- colnames(ssr)[1:5]
  
  ylims <- c(min(WW, xdata, na.rm=TRUE), max(WW, xdata, na.rm=TRUE))
  
  pdf(fnam, height=8.27, width=11.69)
    plot(Wd, WW, type="l", ylim=ylims, lwd=2, sub=xsername,
         xlab="", ylab="", main=jnam)
    lines(Wd, xdata[,1], lwd=2, col=xcols[1])
    lines(Wd, xdata[,2], lwd=2, col=xcols[2])
    lines(Wd, xdata[,3], lwd=2, col=xcols[3])
    lines(Wd, xdata[,4], lwd=2, col=xcols[4])
    lines(Wd, xdata[,5], lwd=2, col=xcols[5])
    # lines(Wd, xdata[,6], lwd=2, col=xcols[6])
    legend("topleft", legend=colnames(xdata), col=xcols, lwd=rep(2,NCOL(xdata)), cex=0.6)
  dev.off()
}

setwd(saveindir2)
write.csv(ssr[order(rownames(ssr)),], paste(xsername, "-ssr.csv", sep=""))
write.csv(bestssr, paste(xsername, "-bestssr.csv", sep=""))

prevdat <- which(Yd==Wd[1])-1
WW <- rbind(Y[prevdat,], W)
rownames(WW) <- c(as.character(Yd[prevdat]), rownames(W))
Wd <- as.Date(rownames(WW))

for(j in 1:NROW(anams))
{
  jnam <- anams[j]
  fnam <- paste("zfig-CI-", xsername,"-", jnam, ".pdf", sep="")
  
  # xdata <- cbind(for5p[,jnam], for4p[,jnam], for3p[,jnam],
  #                for2p[,jnam], for1p[,jnam], for0p[,jnam])
  
  xdata <- cbind(for5p[,jnam], for4p[,jnam], for3p[,jnam],
                 for2p[,jnam], for1p[,jnam])
  xdata <- rbind(matrix(NA, 1, NCOL(xdata)), xdata)
  rownames(xdata) <- rownames(WW)
  colnames(xdata) <- colnames(ssr)[1:5]
  
  # Now for the cloud of the -3w release
  xcloud <- all3f[rownames(WW),,jnam]
  
  ylims <- c(min(WW, xdata, xcloud, na.rm=TRUE), max(WW, xdata, xcloud, na.rm=TRUE))
  
  pdf(fnam, height=8.27, width=11.69)
  plot(Wd, WW, type="l", ylim=ylims, lwd=2, sub=xsername,
       xlab="", ylab="", main=jnam)
  polygon(c(Wd, rev(Wd)), c(xcloud[,"0.05"], rev(xcloud[,"0.95"])), col = '#CCE5FF', border = NA)
  polygon(c(Wd, rev(Wd)), c(xcloud[,"0.2"], rev(xcloud[,"0.8"])), col = '#99CCFF', border = NA)
  lines(Wd, WW, lwd=2, col="black")
  lines(Wd, xdata[,1], lwd=2, col=xcols[1])
  lines(Wd, xdata[,2], lwd=2, col=xcols[2])
  lines(Wd, xdata[,3], lwd=2, col=xcols[3])
  lines(Wd, xdata[,4], lwd=2, col=xcols[4])
  lines(Wd, xdata[,5], lwd=2, col=xcols[5])
  #lines(Wd, xdata[,6], lwd=2, col=xcols[6])
  legend("topleft", legend=colnames(xdata), col=xcols, lwd=rep(2,NCOL(xdata)), cex=0.6)
  dev.off()
}

# Finally, we prepare some more tables
# Tables with frequencies that true value is inside the XX bounds
# for 5w, 3w, 2w, 1w, 0w
#
# Then, we also do the LR test of BErkowitz
# pissrA: 0.05 / 0.95
# pissrB: 0.1 / 0.9
# pissrC: 0.2 / 0.8
pissrA <- matrix(NA, NROW(mae), 6)
rownames(pissrA) <- rownames(mae)
colnames(pissrA) <- colnames(mae)

pissrB <- pissrA
pissrC <- pissrA

for(j in 1:NROW(anams))
{
  jnam <- anams[j]
  
  k1 <- "0.05"; k2 <- "0.95"
  xcloud <- all5f[rownames(W),c(k1, k2),jnam]; xtemp <- NULL; for(jj in 1:NROW(xcloud)){if((W[jj]>=xcloud[jj,1])&(W[jj]<=xcloud[jj,2])){ xtemp <- c(xtemp, 1)} else { xtemp <- c(xtemp, 0) } }
  pissrA[j,1] <- mean(xtemp)
  
  xcloud <- all4f[rownames(W),c(k1, k2),jnam]; xtemp <- NULL; for(jj in 1:NROW(xcloud)){if((W[jj]>=xcloud[jj,1])&(W[jj]<=xcloud[jj,2])){ xtemp <- c(xtemp, 1)} else { xtemp <- c(xtemp, 0) } }
  pissrA[j,2] <- mean(xtemp)
  
  xcloud <- all3f[rownames(W),c(k1, k2),jnam]; xtemp <- NULL; for(jj in 1:NROW(xcloud)){if((W[jj]>=xcloud[jj,1])&(W[jj]<=xcloud[jj,2])){ xtemp <- c(xtemp, 1)} else { xtemp <- c(xtemp, 0) } }
  pissrA[j,3] <- mean(xtemp)
  
  xcloud <- all2f[rownames(W),c(k1, k2),jnam]; xtemp <- NULL; for(jj in 1:NROW(xcloud)){if((W[jj]>=xcloud[jj,1])&(W[jj]<=xcloud[jj,2])){ xtemp <- c(xtemp, 1)} else { xtemp <- c(xtemp, 0) } }
  pissrA[j,4] <- mean(xtemp)
  
  xcloud <- all1f[rownames(W),c(k1, k2),jnam]; xtemp <- NULL; for(jj in 1:NROW(xcloud)){if((W[jj]>=xcloud[jj,1])&(W[jj]<=xcloud[jj,2])){ xtemp <- c(xtemp, 1)} else { xtemp <- c(xtemp, 0) } }
  pissrA[j,5] <- mean(xtemp)
  
  xcloud <- all0f[rownames(W),c(k1, k2),jnam]; xtemp <- NULL; for(jj in 1:NROW(xcloud)){if((W[jj]>=xcloud[jj,1])&(W[jj]<=xcloud[jj,2])){ xtemp <- c(xtemp, 1)} else { xtemp <- c(xtemp, 0) } }
  pissrA[j,6] <- mean(xtemp)
  
  k1 <- "0.1"; k2 <- "0.9"
  xcloud <- all5f[rownames(W),c(k1, k2),jnam]; xtemp <- NULL; for(jj in 1:NROW(xcloud)){if((W[jj]>=xcloud[jj,1])&(W[jj]<=xcloud[jj,2])){ xtemp <- c(xtemp, 1)} else { xtemp <- c(xtemp, 0) } }
  pissrB[j,1] <- mean(xtemp)
  
  xcloud <- all4f[rownames(W),c(k1, k2),jnam]; xtemp <- NULL; for(jj in 1:NROW(xcloud)){if((W[jj]>=xcloud[jj,1])&(W[jj]<=xcloud[jj,2])){ xtemp <- c(xtemp, 1)} else { xtemp <- c(xtemp, 0) } }
  pissrB[j,2] <- mean(xtemp)
  
  xcloud <- all3f[rownames(W),c(k1, k2),jnam]; xtemp <- NULL; for(jj in 1:NROW(xcloud)){if((W[jj]>=xcloud[jj,1])&(W[jj]<=xcloud[jj,2])){ xtemp <- c(xtemp, 1)} else { xtemp <- c(xtemp, 0) } }
  pissrB[j,3] <- mean(xtemp)
  
  xcloud <- all2f[rownames(W),c(k1, k2),jnam]; xtemp <- NULL; for(jj in 1:NROW(xcloud)){if((W[jj]>=xcloud[jj,1])&(W[jj]<=xcloud[jj,2])){ xtemp <- c(xtemp, 1)} else { xtemp <- c(xtemp, 0) } }
  pissrB[j,4] <- mean(xtemp)
  
  xcloud <- all1f[rownames(W),c(k1, k2),jnam]; xtemp <- NULL; for(jj in 1:NROW(xcloud)){if((W[jj]>=xcloud[jj,1])&(W[jj]<=xcloud[jj,2])){ xtemp <- c(xtemp, 1)} else { xtemp <- c(xtemp, 0) } }
  pissrB[j,5] <- mean(xtemp)
  
  xcloud <- all0f[rownames(W),c(k1, k2),jnam]; xtemp <- NULL; for(jj in 1:NROW(xcloud)){if((W[jj]>=xcloud[jj,1])&(W[jj]<=xcloud[jj,2])){ xtemp <- c(xtemp, 1)} else { xtemp <- c(xtemp, 0) } }
  pissrB[j,6] <- mean(xtemp)
  
  k1 <- "0.2"; k2 <- "0.8"
  xcloud <- all5f[rownames(W),c(k1, k2),jnam]; xtemp <- NULL; for(jj in 1:NROW(xcloud)){if((W[jj]>=xcloud[jj,1])&(W[jj]<=xcloud[jj,2])){ xtemp <- c(xtemp, 1)} else { xtemp <- c(xtemp, 0) } }
  pissrC[j,1] <- mean(xtemp)
  
  xcloud <- all4f[rownames(W),c(k1, k2),jnam]; xtemp <- NULL; for(jj in 1:NROW(xcloud)){if((W[jj]>=xcloud[jj,1])&(W[jj]<=xcloud[jj,2])){ xtemp <- c(xtemp, 1)} else { xtemp <- c(xtemp, 0) } }
  pissrC[j,2] <- mean(xtemp)
  
  xcloud <- all3f[rownames(W),c(k1, k2),jnam]; xtemp <- NULL; for(jj in 1:NROW(xcloud)){if((W[jj]>=xcloud[jj,1])&(W[jj]<=xcloud[jj,2])){ xtemp <- c(xtemp, 1)} else { xtemp <- c(xtemp, 0) } }
  pissrC[j,3] <- mean(xtemp)
  
  xcloud <- all2f[rownames(W),c(k1, k2),jnam]; xtemp <- NULL; for(jj in 1:NROW(xcloud)){if((W[jj]>=xcloud[jj,1])&(W[jj]<=xcloud[jj,2])){ xtemp <- c(xtemp, 1)} else { xtemp <- c(xtemp, 0) } }
  pissrC[j,4] <- mean(xtemp)
  
  xcloud <- all1f[rownames(W),c(k1, k2),jnam]; xtemp <- NULL; for(jj in 1:NROW(xcloud)){if((W[jj]>=xcloud[jj,1])&(W[jj]<=xcloud[jj,2])){ xtemp <- c(xtemp, 1)} else { xtemp <- c(xtemp, 0) } }
  pissrC[j,5] <- mean(xtemp)
  
  xcloud <- all0f[rownames(W),c(k1, k2),jnam]; xtemp <- NULL; for(jj in 1:NROW(xcloud)){if((W[jj]>=xcloud[jj,1])&(W[jj]<=xcloud[jj,2])){ xtemp <- c(xtemp, 1)} else { xtemp <- c(xtemp, 0) } }
  pissrC[j,6] <- mean(xtemp)
}

pissrA <- round(pissrA*100,2)
pissrB <- round(pissrB*100,2)
pissrC <- round(pissrC*100,2)

write.csv(pissrA[order(rownames(pissrA)),], paste(xsername, "-pissrA-5-95.csv", sep=""))
write.csv(pissrB[order(rownames(pissrB)),], paste(xsername, "-pissrB-10-90.csv", sep=""))
write.csv(pissrC[order(rownames(pissrC)),], paste(xsername, "-pissrC-20-80.csv", sep=""))


# Now calculate Berkowitz
berk <- matrix(NA, NROW(mae), 6)
rownames(berk) <- rownames(mae)
colnames(berk) <- colnames(mae)

for(j in 1:NROW(anams))
{
  jnam <- anams[j]
  
  xcloud <- all5f[rownames(W),,jnam]; z <- pnorm(xcloud[,1], mean=apply(xcloud[,2:NCOL(xcloud)], 1, mean), sd=apply(xcloud[,2:NCOL(xcloud)], 1, sd)); Z <- qnorm(z)
  berk[j,1] <- BerkowitzTest(Z, lags=1, significance = 0.05)$LRp
  
  xcloud <- all4f[rownames(W),,jnam]; z <- pnorm(xcloud[,1], mean=apply(xcloud[,2:NCOL(xcloud)], 1, mean), sd=apply(xcloud[,2:NCOL(xcloud)], 1, sd)); Z <- qnorm(z)
  berk[j,2] <- BerkowitzTest(Z, lags=1, significance = 0.05)$LRp
  
  xcloud <- all3f[rownames(W),,jnam]; z <- pnorm(xcloud[,1], mean=apply(xcloud[,2:NCOL(xcloud)], 1, mean), sd=apply(xcloud[,2:NCOL(xcloud)], 1, sd)); Z <- qnorm(z)
  berk[j,3] <- BerkowitzTest(Z, lags=1, significance = 0.05)$LRp
  
  xcloud <- all2f[rownames(W),,jnam]; z <- pnorm(xcloud[,1], mean=apply(xcloud[,2:NCOL(xcloud)], 1, mean), sd=apply(xcloud[,2:NCOL(xcloud)], 1, sd)); Z <- qnorm(z)
  berk[j,4] <- BerkowitzTest(Z, lags=1, significance = 0.05)$LRp
  
  xcloud <- all1f[rownames(W),,jnam]; z <- pnorm(xcloud[,1], mean=apply(xcloud[,2:NCOL(xcloud)], 1, mean), sd=apply(xcloud[,2:NCOL(xcloud)], 1, sd)); Z <- qnorm(z)
  berk[j,5] <- BerkowitzTest(Z, lags=1, significance = 0.05)$LRp
  
  xcloud <- all0f[rownames(W),,jnam]; z <- pnorm(xcloud[,1], mean=apply(xcloud[,2:NCOL(xcloud)], 1, mean), sd=apply(xcloud[,2:NCOL(xcloud)], 1, sd)); Z <- qnorm(z)
  berk[j,6] <- BerkowitzTest(Z, lags=1, significance = 0.05)$LRp
}
berk <- round(berk, 3)

write.csv(berk[order(rownames(berk)),], paste(xsername, "-berk.csv", sep=""))















