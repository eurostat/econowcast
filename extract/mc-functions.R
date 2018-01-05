# Standardisation of dataset
xstd <- function(x)
{
  xf <- x
  for(i in 1:NCOL(xf))
  {
    xf[,i] <- (xf[,i]-mean(xf[,i], na.rm=T))/sd(xf[,i], na.rm=T)
  }
  return(xf)
}

# Lagging variables
lagf <- function(x,max.lag) embed(c(rep(NA,max.lag), x), max.lag+1)

# Elbow
k.elbow <- function(mydata, maxc)
{
  p.exp <- rep(NA,maxc)
  for(i in 1:maxc){
    fit <- kmeans(mydata, centers=i)
    p.exp[i] <- 1- fit$tot.withinss / fit$totss
  }
  kout <- min(which(p.exp > 0.9))
  return(kout)
}

k.elbow2 <- function(mydata, maxc)
{
  p.exp <- rep(NA,maxc)
  for(i in 1:maxc){
    fit <- kmeans(mydata, centers=i)
    p.exp[i] <- 1- fit$tot.withinss / fit$totss
    if(p.exp[i] > 0.9){
      break
    }
  }
  kout <- i
  return(kout)
}

