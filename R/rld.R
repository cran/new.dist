#' Inferences on stress–strength reliability based on ranked set sampling data
#' incase of Lindley distribution
#' @export
#' @name rld
#' @param x,q vector of quantiles.
#' @param theta a parameter.
#' @param p vector of probabilities.
#' @param n number of observations. If \code{length(n) > 1}, the length is taken
#'  to be the number required.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are
#' \eqn{P\left[ X\leq x\right]}, otherwise,\eqn{P\left[ X>x\right] }.
#' @description
#' Density, distribution function, quantile function and random generation for
#' Inferences on stress–strength reliability based on ranked set sampling data
#' incase of Lindley distributions parameter.
#' @return \code{drld} gives the density, \code{prld} gives the distribution
#' function, \code{qrld} gives the quantile function and \code{rrld} generates
#' random deviates.
#' @details
#' Inferences on stress–strength reliability based on ranked set sampling data
#' incase of Lindley distribution with a parameter \eqn{\theta}, has density
#' given by
#' \deqn{f\left( x\right) =\frac{\theta ^{2}}{1+\theta }\left( 1+x\right)
#' e^{-\theta~x},}
#' where
#' \deqn{x>0,~\theta >0.}
#' @references  Akgül, F. G., Acıtaş, Ş. ve Şenoğlu, B., 2018,
#' *Inferences on stress–strength reliability based on ranked set sampling data
#' in case of Lindley distribution*, Journal of statistical computation and
#' simulation, 88 (15), 3018-3032.
#' @examples
#' library(new.dist)
#' drld(1,theta=2)
drld<-function(x,theta,log=FALSE)
{
  if(any(theta<=0)) {stop("theta must be > 0")}
  enuzun <- max(length(x),length(theta))
  x<-rep(x,enuzun/length(x)+1)[1:enuzun]
  theta<-rep(theta,enuzun/length(theta)+1)[1:enuzun]
  pdf<-NULL
  for (i in 1:enuzun)
  {
    if(x[i]<=0) {pdf[i]<-0} else
    {pdf[i]<-(theta[i]^2/(1+theta[i]))*(1+x[i])*exp(-theta[i]*x[i])}
  }
  if(log==TRUE) pdf<-log(pdf)
  return(pdf)
}
#' Inferences on stress–strength reliability based on ranked set sampling data
#' incase of Lindley distribution
#' @export
#' @rdname rld
#' @examples
#' prld(1,theta=2)
prld<-function(q,theta,lower.tail=TRUE,log.p=FALSE)
{
  if(any(theta<=0)) {stop("theta must be > 0")}
  enuzun <- max(length(q),length(theta))
  q<-rep(q,enuzun/length(q)+1)[1:enuzun]
  theta<-rep(theta,enuzun/length(theta)+1)[1:enuzun]
  cdf<-NULL
  for (i in 1:enuzun)
  {
    if(q[i]>0) cdf[i]<-1-(1+(theta[i]/(1+theta[i]))*q[i])*exp(-theta[i]*q[i])
    else cdf[i]<-0
  }
  if(lower.tail==FALSE) cdf<-1-cdf
  if(log.p==TRUE) cdf<-log(cdf)
  return(cdf)
}
#' Inferences on stress–strength reliability based on ranked set sampling data
#' incase of Lindley distribution
#' @export
#' @rdname rld
#' @examples
#' qrld(.8,theta=1)
qrld<-function(p,theta,lower.tail=TRUE)
{
  if(any(p<0)|any(p>1)) {stop("p must be between >= 0 and <= 1")}
  if(any(theta<=0)) {stop("theta must be > 0")}
  enuzun <- max(length(p),length(theta))
  p<-rep(p,enuzun/length(p)+1)[1:enuzun]
  theta<-rep(theta,enuzun/length(theta)+1)[1:enuzun]
  qfonk<-NULL
  for (i in 1:enuzun)
  {
    qfonk[i]<--((pracma::lambertWn((1+theta[i])*(-1+p[i])*exp(-1-theta[i]))+
                   1+theta[i])/theta[i])
  }
  if(lower.tail==FALSE)
  {
    qfonk[i]<--((pracma::lambertWn((1+theta[i])*(-1+(1-p[i]))*exp(-1-theta[i]))+
                   1+theta[i])/theta[i])
  }
  return(qfonk)
}
#' Inferences on stress–strength reliability based on ranked set sampling data
#' incase of Lindley distribution
#' @export
#' @rdname rld
#' @examples
#' rrld(10,theta=1)
rrld<-function(n,theta)
{
  n<-floor(n)
  if(any(n<1)) {stop("n must be >= 1")}
  if(any(theta<=0)) {stop("theta must be > 0")}
  rn<-qrld(stats::runif(n),theta)
  return(rn)
}
