# Auto-Arima
umodel <- "AutoArima"
fit <- auto.arima(Yin)
fout <- forecast(fit, h=1, bootstrap=TRUE, npaths=B, level=seq(51,99,1), fan=FALSE)
zout <- c(fout$mean, rev(as.numeric(fout$lower))[1], rev(fout$lower),
          fout$mean, fout$upper, as.numeric(fout$upper)[49])
uni3w[as.character(uD[i]), ,umodel] <- zout

# baggedETS
umodel <- "baggedETS"
fit <- baggedETS(Yin)
fout <- forecast(fit, h=1, level=seq(51,99,1), fan=FALSE)
sigmah <- sd(fit$residuals)
spseq <- qnorm(seq(0.51, 0.99, 0.01))*sigmah
fout <- as.numeric(fout$mean)
zout <- c(fout, fout-rev(spseq)[1], fout-rev(spseq), fout,
          fout+spseq, fout+spseq[NROW(spseq)])
uni3w[as.character(uD[i]), ,umodel] <- zout

# bats
umodel <- "bats"
fit <- bats(Yin)
fout <- forecast(fit, h=1, bootstrap=TRUE, npaths=B, level=seq(51,99,1), fan=FALSE)
zout <- c(fout$mean, rev(as.numeric(fout$lower))[1], rev(fout$lower),
          fout$mean, fout$upper, as.numeric(fout$upper)[49])
uni3w[as.character(uD[i]), ,umodel] <- zout

# ets
umodel <- "ets"
fit <- ets(Yin)
fout <- forecast(fit, h=1, bootstrap=TRUE, npaths=B, level=seq(51,99,1), fan=FALSE)
zout <- c(fout$mean, rev(as.numeric(fout$lower))[1], rev(fout$lower),
          fout$mean, fout$upper, as.numeric(fout$upper)[49])
uni3w[as.character(uD[i]), ,umodel] <- zout

# nnetar
umodel <- "nnetar"
fit <- nnetar(Yin)
fout <- forecast(fit, h=1, level=seq(51,99,1), fan=FALSE)
sigmah <- sd(fit$residuals, na.rm=TRUE)
spseq <- qnorm(seq(0.51, 0.99, 0.01))*sigmah
fout <- as.numeric(fout$mean)
zout <- c(fout, fout-rev(spseq)[1], fout-rev(spseq), fout,
          fout+spseq, fout+spseq[NROW(spseq)])
uni3w[as.character(uD[i]), ,umodel] <- zout

# spline
umodel <- "spline"
fout <- splinef(Yin, h=1, level=seq(51,99,1), fan=FALSE)
zout <- c(fout$mean, rev(as.numeric(fout$lower))[1], rev(fout$lower),
          fout$mean, fout$upper, as.numeric(fout$upper)[49])
uni3w[as.character(uD[i]), ,umodel] <- zout

# tbats
umodel <- "tbats"
fit <- tbats(Yin)
fout <- forecast(fit, h=1, bootstrap=TRUE, npaths=B, level=seq(51,99,1), fan=FALSE)
zout <- c(fout$mean, rev(as.numeric(fout$lower))[1], rev(fout$lower),
          fout$mean, fout$upper, as.numeric(fout$upper)[49])
uni3w[as.character(uD[i]), ,umodel] <- zout

# theta
umodel <- "theta"
fout <- thetaf(Yin, h=1, level=seq(51,99,1), fan=FALSE)
zout <- c(fout$mean, rev(as.numeric(fout$lower))[1], rev(fout$lower),
          fout$mean, fout$upper, as.numeric(fout$upper)[49])
uni3w[as.character(uD[i]), ,umodel] <- as.numeric(zout)