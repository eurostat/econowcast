rm(list=ls())
# Bayesian VAR
install.packages("MSBVAR") # download and install package

library (MSBVAR)

#generate some artificial data
set.seed(1)
n = 200 #sample size
p = 10 # number of variables
X0 = rep(0,p)
beta = rep(0.5,p)
B=diag(beta)
y=matrix(0,nrow=p,ncol=n)
for (i in 1:n) {
e = rnorm(p)
y[,i]=B%*%X0+e
X0=y[,i]
}

# Reference prior model -- Normal-IW prior pdf
Bvar.Model <- szbvar(y, p=6, z=NULL, lambda0=0.6,
lambda1=0.1, lambda3=2, lambda4=0.5,
lambda5=0, mu5=0, mu6=0,
nu=ncol(KEDS)+1, qm=4, prior=0,
posterior.fit=F)

# Forecast -- this gives back the sample PLUS the forecasts.
forecasts <- forecast(Bvar.Model, nsteps=10,burnin=3000, gibbs=5000, exog=NULL)

# Conditional forecasts
conditional.forcs.ref <- hc.forecast(Bvar.Model, yhat, nsteps,
burnin=3000, gibbs=5000, exog=NULL)

