library(RTMB)

source("recdata.R")

par <- list(logRmax=0, logS50=0, logSigma=0)
par <- unlist(par)

f <- function(par)
{
  Rmax <- exp(par[["logRmax"]])
  S50 <- exp(par[["logS50"]])
  sigma <- exp(par[["logSigma"]])

  S <- recdata$S
  R <- recdata$R

  Rhat <- Rmax * S / (S + S50)
  nll <- -sum(dnorm(log(R), log(Rhat), sigma, TRUE))
  nll
}

obj <- MakeADFun(f, par)

opt <- nlminb(obj$par, obj$fn, obj$gr)

nlminb(par, f)
