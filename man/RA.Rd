% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RA.R
\name{RA}
\alias{RA}
\alias{dRA}
\alias{pRA}
\alias{qRA}
\alias{rRA}
\title{Ram Awadh Distribution}
\usage{
dRA(x, theta = 1, log = FALSE)

pRA(q, theta = 1, lower.tail = TRUE, log.p = FALSE)

qRA(p, theta = 1, lower.tail = TRUE)

rRA(n, theta = 1)
}
\arguments{
\item{x, q}{vector of quantiles.}

\item{theta}{a scale parameter.}

\item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}

\item{lower.tail}{logical; if TRUE (default), probabilities are
\eqn{P\left[ X\leq x\right]}, otherwise, \eqn{P\left[ X>x\right] }.}

\item{p}{vector of probabilities.}

\item{n}{number of observations. If \code{length(n) > 1}, the length is taken
to be the number required.}
}
\value{
\code{dRA} gives the density, \code{pRA} gives the distribution
function, \code{qRA} gives the quantile function and \code{rRA}
generates random deviates.
}
\description{
Density, distribution function, quantile function and random generation for
a Ram Awadh distribution with parameter \code{scale}.
}
\details{
Ram Awadh distribution with \code{scale} parameter
\eqn{\theta}, has density
\deqn{f\left( x\right) =\frac{\theta ^{6}}{\theta ^{6}+120}
 \left( \theta+x^{5}\right) e^{-\theta x},}
where
\deqn{x>0,~\theta >0.}
}
\examples{
library(new.dist)
dRA(1,theta=2)
pRA(1,theta=2)
qRA(.1,theta=1)
rRA(10,theta=1)
}
\references{
Shukla, K. K., Shanker, R. ve Tiwari, M. K., 2022,
\emph{A new one parameter discrete distribution and its applications}, Journal
of Statistics and Management Systems, 25 (1), 269-283.
}
