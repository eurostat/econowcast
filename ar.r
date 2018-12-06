if(arp==0){
  arp <- NROW(ar.ols(z, aic=TRUE)$ar)
}
fit <- Arima(z,order=c(arp,0,0), method=c("CSS"))
fout <- NULL
try(fout <- forecast(fit,h=1, bootstrap=TRUE, npaths=B, level=seq(51,99,1)), silent=TRUE)

while(is.null(fout)==TRUE)
{
arp <- arp-1
fit <- Arima(z,order=c(arp,0,0), method=c("CSS"))
try(fout <- forecast(fit,h=1, bootstrap=TRUE, npaths=B, level=seq(51,99,1)), silent=TRUE)
}

zout <- c(fout$mean, rev(as.numeric(fout$lower))[1], rev(fout$lower),
          fout$mean, fout$upper, as.numeric(fout$upper)[49])

if(YTRANSF==3){
  zlast <- YSAV[NROW(YSAV)]
  zout <- zlast*(1+zout)
}
if(YTRANSF==2){
  zlast <- YSAV[NROW(YSAV)]
  zout <- zlast + zout
}

