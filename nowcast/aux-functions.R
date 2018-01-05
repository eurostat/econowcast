# strip dataset from its attributes and have the data matrices ready
stripatts <- function(zz)
{
  z <- as.matrix(zz)
  cz <- z[1:3,]; czn <- cz[,1]; 
  cz <- apply(cz[,2:NCOL(cz)], 2, as.numeric); 
  rownames(cz) <- czn
  z <- z[4:NROW(z),]
  dz <- z[,1]
  z <- z[,2:NCOL(z)]
  z <- apply(z, 2, as.numeric)
  rownames(z) <- dz
  return(list(data=z, atts=cz))
}

# Transform data according to the legend
transformdata <- function(v, vcode)
{
  vv <- matrix(NA, NROW(v), NCOL(v))
  for(jj in 1:NCOL(v))
  {
    transf <- vcode[3,jj]
    # no change
    if(transf==1){
      vv[,jj] <- v[,jj]
    }
    
    # First dif
    if(transf==2){
      vv[,jj] <- c(NA, v[2:NROW(v),jj]-v[1:(NROW(v)-1),jj])
    }
    
    # p. change
    if(transf==3){
      vv[,jj] <- c(NA, (v[2:NROW(v),jj]/v[1:(NROW(v)-1),jj])-1)
    }
    
    # log
    if(transf==4){
      vv[,jj] <- log(v[,jj])
    }
    
    # log diff
    if(transf==5){
      vtemp <- log(v[,jj])
      vv[,jj] <- c(NA, (vtemp[2:NROW(vtemp)]/vtemp[1:(NROW(vtemp)-1)])-1)
    }
    
    # diff of log diff
    # if(transf==6){
    #   vtemp <- log(v[,jj])
    #   vtemp2 <- c(NA, (vtemp[2:NROW(vtemp)]/vtemp[1:(NROW(vtemp)-1)])-1)
    #   vv[,jj] <- c(NA, (vtemp2[2:NROW(vtemp2)]/vtemp2[1:(NROW(vtemp2)-1)])-1)
    # }
    # because the last transf does not always wrk
    # we switch to p change
    if(transf==6){
      vv[,jj] <- c(NA, (v[2:NROW(v),jj]/v[1:(NROW(v)-1),jj])-1)
    }
  }
  rownames(vv) <- rownames(v)
  colnames(vv) <- colnames(v)
  return(vv)
}


givemonth <- function(um)
{
  ui <- um
  if(ui==1){ ui <- "01" }
  if(ui==2){ ui <- "02" }
  if(ui==3){ ui <- "03" }
  if(ui==4){ ui <- "04" }
  if(ui==5){ ui <- "05" }
  if(ui==6){ ui <- "06" }
  if(ui==7){ ui <- "07" }
  if(ui==8){ ui <- "08" }
  if(ui==9){ ui <- "09" }
  if(ui==10){ ui <- "10" }
  if(ui==11){ ui <- "11" }
  if(ui==12){ ui <- "12" }
  return(ui)
}


weeklytomonthly <- function(ys, zws)
{
  md <- as.Date(rownames(ys))
  v <- matrix(NA, NROW(md), NCOL(zws))
  for(j in 1:NCOL(zws))
  {
    vtemp <- as.matrix(zws[,j])
    for(jj in 1:NROW(md))
    {
      ddseq <- seq(floor_date(md[jj], "month"), md[jj], 1) 
      # count Saturdays when we have the weekly google trends
      dd <- ddseq[which(wday(ddseq)==7)]
      dd <- dd[dd<=max(as.Date(rownames(zws)))]
      
      v[jj,j] <- mean(vtemp[as.character(dd),], na.rm=TRUE)
    }
  }
  colnames(v) <- colnames(zws)
  rownames(v) <- rownames(y)
  return(v)
}

shiftNAs <- function(k)
{
  kout <- k
  for(j in 1:NCOL(k))
  {
    kna <- sum(is.na(k[20:NROW(k),j]))
    if(kna>0){
      kout[,j] <- c(rep(NA, kna), k[1:(NROW(k)-kna),j])
    }
  }
  colnames(kout) <- colnames(k)
  rownames(kout) <- rownames(k)
  return(kout)
}


# Simple MA (centered)
MAcentered <- function(y, k)
{
  yy <- c(rep(y[1], k), y, rep(y[NROW(y)], k))
  yyy <- yy
  for(i in (k+1):(NROW(yy)-k))
  {
    yyy[i-k] <- mean(yy[(i-k):(i+k)])
  }
  return(yyy)
}

# Define outliers as those obs. that exceed 4 times the interquartile
# Distance
outliers_correction <- function(X)
{
  Jmis <- which(is.na(X)==TRUE)
  Jout <- (abs(X-quantile(X, probs=1/2, na.rm=TRUE))>4*abs(quantile(X, probs=1/4, na.rm=TRUE)-quantile(X, probs=0, na.rm=TRUE)))
  Jout <- as.double(Jout)
  Jout[Jmis] <- 0
  Jout <- which(Jout==1)
  
  Z <- X
  Z[Jmis] <- median(X, na.rm=TRUE) # put the median in place of missing values
  Z[Jout] <- median(X, na.rm=TRUE) # put the median in place of outliers
  
  Zma <- MAcentered(Z,3)
  
  Z[Jout] <- Zma[Jout]
  Z[Jmis] <- Zma[Jmis]
  
  return(Z)
}

outliers_correction2 <- function(X)
{
  Jmis <- which(is.na(X)==TRUE)
  Jout <- c(which(X>(median(X, na.rm=TRUE)+3*sd(X, na.rm=TRUE))),
            which(X<(median(X, na.rm=TRUE)-3*sd(X, na.rm=TRUE))))
  
  Z <- X
  Z[Jmis] <- median(X, na.rm=TRUE) # put the median in place of missing values
  Z[Jout] <- median(X, na.rm=TRUE) # put the median in place of outliers
  
  Zma <- MAcentered(Z,3)
  
  Z[Jout] <- Zma[Jout]
  Z[Jmis] <- Zma[Jmis]
  return(Z)
}


MBB <- function(x, b)
{
  xboot <- NULL
  t <- NROW(x)
  k <- NROW(xboot)
  while(k<=(t+5)){
    length <- b
    point <- runif(1, 1, t-b)
    xstart <- point;
    xend <- point+length
    x_new <- x[(xstart+1):(xend)]
    xboot <- c(xboot, x_new)
    k <- NROW(xboot)
  }
  xboot <- xboot[1:t]
  return(xboot)
}


# Lagging variables
lagf <- function(x,max.lag) embed(c(rep(NA,max.lag), x), max.lag+1)

lagmv <- function(k, klag)
{
  kout <- matrix(NA, NROW(k), 0)
  for(j in 1:NCOL(k))
  {
    ktemp <- lagf(k[,j], klag)
    colnames(ktemp) <- rep(colnames(k)[j], klag+1)
    kout <- cbind(kout, ktemp)
  }
  return(kout)
}


## Load some aux functions
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


# Same code as in Giannone, Reichlin
# f: predictors
# z: target
# nu: shrinkage parameter
# hs: h step ahead
# *** if hs<0 then use all the info. Use this setup for the Auto
bsr <- function(f, z, nu, hs)
{
  # Normalise here
  zmu <- mean(z)
  zsd <- sd(z)
  zz <- (z-zmu)/zsd
  ff <- xstd(f)/(sqrt(NROW(f)*NCOL(f)))
  
  if(hs>0){
    ffu <- ff[1:(NROW(ff)-hs),]
    zzu <- zz[(hs+1):NROW(zz),]
  }else{
    ffu <- ff
    zzu <- zz
  }
  
  NN <- NCOL(f)
  TT <- NROW(f) 
  # Codes as in Giannone-Reichlin
  # // Initialize the Iterative Landweber algorithm
  # Set the threshold
  # thresh <- sqrt(NN*TT)*nu/2
  thresh      <- nu/2  # set the threshold
  tollerance  <- 1e-5  # Tollerance for checking convergence
  max_iter    <- 30 # Maximum number of iterations
  
  # // Initialize the parameters
  fit_prev    <- 1e+32 # Initialize the in sample fit
  Dfit        <- 1e+32 # Initial value for the change in fit, to check convergence;
  b <- matrix(0, NN, 1)
  cont <- 1
  
  # Compute the regression by using the Iterative Landweber scheme with soft thresholding
  while((Dfit>tollerance)&(cont<max_iter))
  {
    cont <- cont+1
    b_temp <- matrix(0, NN, 1)
    
    # performs the Landweber iteration
    temp <- (as.matrix(b[,cont-1])) - t(ffu)%*%ffu%*%b[,cont-1] + t(ffu)%*%(zzu)
    
    # applies the soft thresholding at each iteration
    keep <- matrix(0, NROW(temp), 1)
    for(i in 1:NROW(temp))
    {
      keep[i,1] <- i*(abs(temp[i,1])>thresh) # Keeps the parameters larger than the threeshold (set the remaining to zero)
      if(keep[i,1]>0){
        b_temp[i,1] <- temp[i,1] - thresh*sign(temp[i,1]) # reduces the magnitude of the kept parameters  by threeh
      }else{
        b_temp[i,1] <- 0
      }
    }
    
    b<- cbind(b, b_temp)
    # loss function
    fit <- sd(zzu-ffu%*%b_temp)^2 # Computes the in-sample MSE
    Dfit <- abs(fit-fit_prev)     # Compute the change in forecasts for checking convergence
    fit_prev <- fit
  }
  
  if(cont==max_iter){
    warning("Gianno-Reichlin LASSO: Maximum iteration reached")
  }
  
  if(sum(abs(b_temp))==0){
    # Simple shrinkage regression in case lasso fails
    b_temp <- solve((t(ffu)%*%ffu+nu*diag(1,NCOL(ffu))))%*%(t(ffu)%*%zzu)
    b<- cbind(b, b_temp)
  }
  
  b <- b[,NCOL(b)]
  if(hs>0){
    pred <- (ffu[NROW(ffu),]%*%b)*zsd+zmu
  }else{
    # pred <- c(NA, sd(zzu-ffu%*%b)^2)
    pred <- (ffu[NROW(ffu),]%*%b)*zsd+zmu
  }
  se <- sd(zzu-ffu%*%b)
  return(list(vfor=pred, bhat=b, se=se))
}

# The Set function from the same set of Giannone, Reichlin codes
set.bsr <- function(f, z, hs, K)
{
  nu_min <- 0
  nu_max <- 2
  K_max <-1e+32
  K_min <- 0
  K_avg <- 1e+32
  max_iter <- 1000
  cont <- 0
  
  while((K_min!=K_max)&(cont<max_iter))
  {
    cont <- cont + 1
    nu_avg <- (nu_min+nu_max)/2
    bsr.temp <- bsr(f, z, nu_avg, hs)
    pred <- bsr.temp$vfor
    b <- bsr.temp$bhat
    se <- bsr.temp$se
    
    K_avg <- sum(b!=0)
    
    if(K_avg < K){
      nu_min <- nu_min
      nu_max <- nu_avg
    }else{
      nu_min <- nu_avg
      nu_max <- nu_max
    }
  }
  
  if(cont>=max_iter){
    warning("warning: max iter reached vwhen setting the Lasso penalization")
  }
  
  nu <- nu_avg
  return(list(vfor=pred, bhat=b, nuh=nu, se=se))
}


ricSW <- function(x, q, r, p)
{
  # function [A, C, Q, R, initx, initV, Mx, Wx] = ricSW(x,q,r,p)
  # Computes the parameters of the factor models 
  # REMARK: the parameters C and R refer to the standardized variables.
  
  Mx <- apply(x, 2, mean) # Mean
  Sx <- apply(x, 2, sd)  # Standard deviation
  Wx <- diag(Sx)
  for(j in 1:NCOL(x))
  {
    x[,j] <- (x[,j]-Mx[j])/Sx[j]
  }
  
  x <- x%*%solve(Wx) # Standardize
  
  T <- NROW(x)
  N <- NCOL(x) 
  
  if(r<q){
    # Static rank r cannot be larger than the dynamic rank
    warning('q has to be less or equal to r')
    stop("r<q")
  }
  
  nlag <- p-1 #p=1, so nlag = 0.
  
  # Define some preliminary quantity that are necessary to writhe the VAR in companion form
  A_temp <- t(matrix(0, r, r*p)) # a zero matrix
  I <- diag(r*p)   # identity matrix
  if(p==1){
    A=A_temp # NOTE: if p=1, then I(1:end-r,1:end) is empty. In this case, MATLAB reads A as equal to A_temp.
  }else{
    A=rbind(t(A_temp), I[1:(NROW(I)-r), 1:NCOL(I)])
  }
  
  Q <- matrix(0, r*p, r*p)  # a zero matrix, 10x10.
  Q[1:r,1:r] <- diag(r) # identity of size=10.   
  
  vv <- eigen(cov(x))
  v <- vv$vectors[,1:r]
  d <- vv$values[1:r]
  
  F <- x%*%v # PC estimates of the common factors
  
  R <- matrix(0, N, N)
  diag(R) <- diag(cov(x-x%*%v%*%t(v))) #Estimate of the covariance matrix of the idiosincratic component
  # REMARK: x*v*v'' is the projection of x over the principal components (F=x*v)
  
  # ESTIMATE the AUTOregressive model for the Factors: run the var F(t) = A_1*F(t-1)+...+A_p*F(t-1) + e(t);
  if(p>0){
    
    z <- F
    Z <- matrix(NA, NROW(z)-p, 0)
    for(kk in 1:p)
    {
      Z <- cbind(Z, z[(p-kk+1):(NROW(z)-kk),])
    }
    z <- z[(p+1):NROW(z),]
    
    A_temp <- solve(t(Z)%*%Z)%*%t(Z)%*%z
    A[1:r, 1:(r*p)] <- t(A_temp)
    
    # Compute Q
    e <-  z - Z%*%A_temp # VAR residuals
    H <- cov(e) # VAR covariance matrix
    
    if(r==q){ # The covariance matrix of the VAR residuals is of full rank
      Q[1:r, 1:r] <- H
    }else{    # The covariance matrix of the VAR residuals has reduced rank
      PP <- eigen(H)
      P <- PP$vectors[,1:q]
      M <- PP$values[1:q]
      
      P <- P%*%diag(sign(P[1,]))
      u_orth <- e%*%P*(M^-.5) # extracting the common shocks
      e_pc <- e%*%P%*%t(P)
      Q[1:r,1:r] <- P%*%diag(M)%*%t(P)
    }
  }
  
  
  # Computes the initial conditions for the filter.
  # The common factors are initialized by the PC estimates.
  # Initial variance is set equal to the unconditional variance ofthe common factors. 
  if(p>0){
    z <- F
    Z <- matrix(NA, NROW(z)-nlag, 0)
    for(kk in 0:nlag)
    {
      Z <- cbind(Z, z[(nlag-kk+1):(NROW(z)-kk),])
    }
    initx <- t(as.matrix(Z[1,])) 
    initV <- matrix(ginv(diag(dim(kronecker(A, A))[1])-kronecker(A, A))%*%as.vector(Q), r*p)
  }else{
    initx <- NULL
    initV <- NULL
  }
  
  C <- cbind(v, matrix(0, N, r*nlag))
  return(list(A=A, C=C, Q=Q, R=R, initx=initx, initV=initV, Mx=Mx, Wx=Wx))
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# % Adapted from programs by Zoubin Ghahramani and Geoffrey E. Hinton, available at http://www.gatsby.ucl.ac.uk/ zoubin, 1996.
# % KALMAN_UPDATE Do a one step update of the Kalman filter
# % [xnew, Vnew, loglik] = kalman_update_diag(A, C, Q, R, y, x, V, ...)
# %
# % INPUTS:
#   % A - the system matrix
# % C - the observation matrix 
# % Q - the system covariance 
# % R - the observation covariance
# % y(:)   - the observation at time t
# % x(:) - E[X | y(:, 1:t-1)] prior mean
# % V(:,:) - Cov[X | y(:, 1:t-1)] prior covariance
# %
# % OPTIONAL INPUTS (string/value pairs [default in brackets])
# % 'initial' - 1 means x and V are taken as initial conditions (so A and Q are ignored) [0]
# % 'u'     - u(:) the control signal at time t [ [] ]
# % 'B'     - the input regression matrix
# %
# % OUTPUTS (where X is the hidden state being estimated)
# %  xnew(:) =   E[ X | y(:, 1:t) ] 
# %  Vnew(:,:) = Var[ X(t) | y(:, 1:t) ]
# %  VVnew(:,:) = Cov[ X(t), X(t-1) | y(:, 1:t) ]
# %  loglik = log P(y(:,t) | y(:,1:t-1)) log-likelihood of innovatio
kalman_update_diag <- function(A, C, Q, R, y, x, V, initial)
{
  # function [xnew, Vnew, loglik, VVnew] = kalman_update_diag(A, C, Q, R, y, x, V, varargin)
  # set default params
  u <- NULL
  B <- NULL
  
  if(initial==0){
    if(is.null(u)==TRUE){
      xpred <- A%*%t(x)
    }else{
      xpred <- A%*%t(x) + B%*%t(u)
    }
    xpred <- t(as.matrix(xpred))
    Vpred <- A%*%V%*%t(A) + Q
  }else{
    if(is.null(u)==TRUE){
      xpred <- x
    }else{
      xpred <- x + B%*%u
    }
    Vpred <- V
  }
  
  e <- y-C%*%t(xpred) # error (innovation)
  n <- NROW(e)
  ss <- dim(A)[1]
  
  d <- dim(e)[1]
  
  S <- C%*%Vpred%*%t(C) + R
  GG <- t(C)%*%diag(1/diag(R))%*%C
  
  Sinv <- diag(1/diag(R)) - diag(1/diag(R))%*%C%*%ginv(diag(ss)+Vpred%*%GG)%*%Vpred%*%t(C)%*%diag(1/diag(R)) #  works only with R diagonal
  
  detS <- prod(diag(R))%*%det(diag(ss)+Vpred%*%GG)
  denom <- (2*pi)^(d/2)*sqrt(abs(detS))
  mahal <- sum(t(e)%*%Sinv%*%e)
  loglik <- -0.5*mahal - log(denom)
  
  K <- Vpred%*%t(C)%*%Sinv # Kalman gain matrix
  
  # If there is no observation vector, set K = zeros(ss).
  xnew <- xpred + t(K%*%e) # csi_est(t\t) formula 13.6. 5    
  Vnew <- (diag(ss) - K%*%C)%*%Vpred # P(t\t) formula 13.2.16 hamilton
  VVnew <- (diag(ss) - K%*%C)%*%A%*%V
  
  return(list(xnew=xnew, Vnew=Vnew, loglik=loglik, VVnew=VVnew))
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#   function [x, V, VV, loglik] = kalman_filter_diag(y, A, C, Q, R, init_x, init_V, varargin)
# % Adapted from programs by Zoubin Ghahramani and Geoffrey E. Hinton, available at http://www.gatsby.ucl.ac.uk/ zoubin, 1996.
# % Kalman filter.
# % [x, V, VV, loglik] = kalman_filter_diag(y, A, C, Q, R, init_x, init_V, ...)
# %
# % INPUTS:
#   % y(:,t)   - the observation at time t
# % A - the system matrix
# % C - the observation matrix 
# % Q - the system covariance 
# % R - the observation covariance
# % init_x - the initial state (column) vector 
# % init_V - the initial state covariance 
# %
# % OPTIONAL INPUTS (string/value pairs [default in brackets])
# % 'model' - model(t)=m means use params from model m at time t [ones(1,T) ]
# %     In this case, all the above matrices take an additional final dimension,
# %     i.e., A(:,:,m), C(:,:,m), Q(:,:,m), R(:,:,m).
# %     However, init_x and init_V are independent of model(1).
# % 'u'     - u(:,t) the control signal at time t [ [] ]
# % 'B'     - B(:,:,m) the input regression matrix for model m
# %
# % OUTPUTS (where X is the hidden state being estimated)
# % x(:,t) = E[X(:,t) | y(:,1:t)]
# % V(:,:,t) = Cov[X(:,t) | y(:,1:t)]
# % VV(:,:,t) = Cov[X(:,t), X(:,t-1) | y(:,1:t)] t >= 2
# % loglik = sum{t=1}^T log P(y(:,t))
# %
# % If an input signal is specified, we also condition on it:
#   % e.g., x(:,t) = E[X(:,t) | y(:,1:t), u(:, 1:t)]
# % If a model sequence is specified, we also condition on it:
#   % e.g., x(:,t) = E[X(:,t) | y(:,1:t), u(:, 1:t), m(1:t)]
kalman_filter_diag <- function(y, A, C, Q, R, init_x, init_V)
{
  os <- dim(y)[1]
  T <- dim(y)[2]
  ss <- dim(A)[1]
  
  # set default params
  model <- 1:T
  u <- NULL
  B <- NULL
  ndx <- NULL
  
  x <- matrix(0, ss, T)
  V <- array(0, c(ss, ss, T))
  VV <- array(0, c(ss, ss, T))
  
  loglik <- 0
  
  for(t in 1:T)
  {
    m <- model[t]
    if(t==1){
      prevx <- init_x
      prevV <- init_V
      initial <- 1
    }else{
      prevx <- t(as.matrix(x[,t-1]))
      prevV <- V[,,t-1]
      initial <- 0
    }
    
    kud.out <- kalman_update_diag(A[,,m], C[,,m], Q[,,m], R[,,m], y[,t], prevx, prevV, initial)
    x[,t] <- kud.out$xnew
    V[,,t] <- kud.out$Vnew
    LL <- kud.out$loglik
    VV[,,t] <- kud.out$VVnew
    
    loglik <- loglik + LL
  }
  return(list(x=x,V=V, VV=VV, loglik=loglik))
}

########################################
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# function [xsmooth, Vsmooth, VVsmooth_future] = smooth_update(xsmooth_future, Vsmooth_future, ...
#                                                              xfilt, Vfilt,  Vfilt_future, VVfilt_future, A, Q, B, u)
# % Adapted from programs by Zoubin Ghahramani and Geoffrey E. Hinton, available at http://www.gatsby.ucl.ac.uk/ zoubin, 1996.
# % One step of the backwards RTS smoothing equations.
# % function [xsmooth, Vsmooth, VVsmooth_future] = smooth_update(xsmooth_future, Vsmooth_future, ...
#                                                                %    xfilt, Vfilt,  Vfilt_future, VVfilt_future, A, B, u)
# %
# % INPUTS:
#   % xsmooth_future = E[X_t+1|T]
# % Vsmooth_future = Cov[X_t+1|T]
# % xfilt = E[X_t|t]
# % Vfilt = Cov[X_t|t]
# % Vfilt_future = Cov[X_t+1|t+1]
# % VVfilt_future = Cov[X_t+1,X_t|t+1]
# % A = system matrix for time t+1
# % Q = system covariance for time t+1
# % B = input matrix for time t+1 (or [] if none)
# % u = input vector for time t+1 (or [] if none)
# %
# % OUTPUTS:
#   % xsmooth = E[X_t|T]
# % Vsmooth = Cov[X_t|T]
# % VVsmooth_future = Cov[X_t+1,X_t|T]
# 
# %xpred = E[X(t+1) | t]
smooth_update <- function(xsmooth_future, Vsmooth_future, xfilt, Vfilt,  Vfilt_future, VVfilt_future, A, Q, B, u)
{
  if(is.null(B)==TRUE)
  {
    xpred <- A%*%xfilt
  }else{
    xpred <- A%*%xfilt + B%*%u
  }
  
  Vpred <- A%*%Vfilt%*%t(A) + Q   # % Vpred = Cov[X(t+1) | t]
  J <- Vfilt%*%t(A)%*%ginv(Vpred) # % smoother gain matrix
  xsmooth <- xfilt + J%*%(xsmooth_future - xpred)
  Vsmooth <- Vfilt + J%*%(Vsmooth_future - Vpred)%*%t(J)
  VVsmooth_future <- VVfilt_future + (Vsmooth_future - Vfilt_future)%*%ginv(Vfilt_future)%*%VVfilt_future
  return(list(xsmooth=xsmooth, Vsmooth=Vsmooth, VVsmooth_future=VVsmooth_future))
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# function [xsmooth, Vsmooth, VVsmooth, loglik] = kalman_smoother_diag(y, A, C, Q, R, init_x, init_V, varargin)
# Adapted from programs by Zoubin Ghahramani and Geoffrey E. Hinton, available at http://www.gatsby.ucl.ac.uk/ zoubin, 1996.
# Kalman/RTS smoother.
# [xsmooth, Vsmooth, VVsmooth, loglik] = kalman_smoother_diag(y, A, C, Q, R, init_x, init_V, ...)
#
# The inputs are the same as for kalman_filter.
# The outputs are almost the same, except we condition on y(:, 1:T) (and u(:, 1:T) if specified),
# instead of on y(:, 1:t).
kalman_smoother_diag <- function(y, A, C, Q, R, init_x, init_V)
{
  os <- dim(y)[1]
  T <- dim(y)[2]
  ss <- dim(A)[1]
  
  # set default params
  model <- 1:T
  u <- NULL
  B <- NULL
  
  xsmooth <- matrix(0, ss, T)
  Vsmooth <- array(0, c(ss, ss, T))
  VVsmooth <- array(0, c(ss, ss, T))
  
  # % Forward pass
  kfd.out <- kalman_filter_diag(y, A, C, Q, R, init_x, init_V)
  
  xfilt <- kfd.out$x
  Vfilt <- kfd.out$V
  VVfilt <- kfd.out$VV
  loglik <- kfd.out$loglik
  
  #  Backward pass
  xsmooth[,T] <- xfilt[,T]
  Vsmooth[,,T] <- Vfilt[,,T]
  
  for(t in seq(T-1, 1, -1))
  {
    m <- model[t+1]
    if(is.null(B)==TRUE){
      su.out <- smooth_update(xsmooth[,t+1], Vsmooth[,,t+1], xfilt[,t], Vfilt[,,t],
                              Vfilt[,,t+1], VVfilt[,,t+1], A[,,m], Q[,,m], B, u)
    }else{
      su.out <- smooth_update(xsmooth[,t+1], Vsmooth[,,t+1], xfilt[,t], Vfilt[,,t],
                              Vfilt[,,t+1], VVfilt[,,t+1], A[,,m], Q[,,m], B[,,m], u[,t+1])
    }
    xsmooth[,t] <- su.out$xsmooth
    Vsmooth[,,t] <- su.out$Vsmooth
    VVsmooth[,,t+1] <- su.out$VVsmooth_future
  }
  VVsmooth[,,1] <- matrix(0, ss, ss)
  
  return(list(xsmooth=xsmooth, Vsmooth=Vsmooth, VVsmooth=VVsmooth, loglik=loglik))
}

# %%%function [F,VF,A,C,Q,R,initx,initV,ss,MM] = FactorExtraction(x,q,r,p,A,C,Q,R,initx,initV,ss,MM);
# %%% extract common factors from vector of time series possibly unbalanced
# %%% at the end of the sample, (NaN for missing observations)
# 
# %% The model
# %% x_t = C F_t + \xi_t
# %% F_t = AF_{t-1} + B u_t
# %% R = E(\xi_t \xi_t')
#          %% Q = BB'
#          %% u_t ~ WN(0,I_q)
#          %% initx = F_0
#          %% initV = E(F_0 F_0')
#                       %% ss: std(x) 
#                       %% MM: mean(x)
#                       
#                       %% q: dynamic rank
#                       %% r: static rank (r>=q)
#                       %% p: ar order of the state vector (default p=1)
#                       
#                       %% F : estimated factors
#                       %% VF: estimation variance for the common factors
FactorExtraction <- function(x,q,r,p)
{
  T <- NROW(x)
  N <- NCOL(x) #dimension of the panel
  
  # Construct the balanced panel z from the original panel x
  # NOTES: sum(isnan(x)) computes the number of NaNs in each column 
  # of x and stores that number in a cell in a row vector, das.
  das <- colSums(is.na(x))
  m <- max(das)
  
  # Estimate the parameters by simple regrssion on
  # Principal components estimates of the common factors
  # (based on the balanced part of the panel)
  
  # z is the matrix with # of rows = T-m (all rows with any Na's are excluded)
  z <- x[1:(T-m),]
  
  ss <- apply(z, 2, sd)  # computes stdev of each column of data.
  MM <- apply(z, 2, mean)
  
  # STEP:  Standardize the panel
  for(j in 1:NCOL(z))
  {
    x[,j] <- (x[,j]-MM[j])/ss[j]
  }
  
  z <- x[1: (T-m),]
  
  ricSWout <- ricSW(z, q, r, p)
  
  A <- ricSWout$A
  C <- ricSWout$C
  Q <- ricSWout$Q
  R <- ricSWout$R
  initx <- ricSWout$initx
  initV <- ricSWout$initV
  
  # The signal extraction in presence of missing data is performed by
  # using a time varying Kalman filter in which missing data are assigned an
  # extremely large variance of the noise in idiosyncratic component.
  
  # Define the parameters of the time varying state space model... time is
  # on the 3rd dimension
  AA <- array(A, c(NROW(A), NCOL(A), T))
  QQ <- array(Q, c(NROW(Q), NCOL(Q), T))
  CC <- array(C, c(NROW(C), NCOL(C), T))
  RR <- array(NA, c(NROW(R), NCOL(R), T))
  
  for(jt in 1:T)
  {
    miss <- which(is.na(x[jt,])==TRUE)
    Rtemp <- diag(R)
    Rtemp[miss] <- 1e+32
    RR[,,jt] <- diag(Rtemp)
  }
  
  # missing data are assigned an arbitrary value...
  xx <- x
  for(j in 1:NCOL(xx))
  {
    mi <- which(is.na(xx[,j])==TRUE)
    xx[mi,j] <- 0
  }
  
  ksd.out <- kalman_smoother_diag(t(xx),AA, CC, QQ, RR, initx, initV)
  
  xsmooth <- ksd.out$xsmooth
  Vsmooth <- ksd.out$Vsmooth
  VVsmooth <- ksd.out$VVsmooth
  loglik <- ksd.out$loglik
  
  VF <- Vsmooth
  ind <- dim(VF)[3]
  Fac <-  t(xsmooth)
  
  return(list(Fac=Fac, VF=VF, A=A, C=C, Q=Q, R=R, initx=initx, initV=initV, ss=ss, MM=MM))
}

beep <- function(n = 3){
  for(i in seq(n)){
    system("rundll32 user32.dll,MessageBeep -1")
    Sys.sleep(.5)
  }
}