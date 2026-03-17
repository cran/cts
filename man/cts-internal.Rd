\name{spec.ci}
\alias{spec.ci}
\title{Internal Function}
\description{Internal Function}
\usage{
spec.ci(spec.obj, coverage = 0.95)
}
\arguments{
  \item{spec.obj}{An object with component \code{df} (degrees of freedom), for example as returned by \code{\link{spec.ls}}.}
  \item{coverage}{Coverage probability for the pointwise confidence interval, in \code{[0,1)}.}
}
\value{A numeric vector of length 2 giving multiplicative factors \code{c(lower, upper)} for pointwise confidence limits for a spectrum estimate.}
\details{This is a small utility used by \code{\link{plotSpecLs}} to draw confidence bars.}
\keyword{internal}
