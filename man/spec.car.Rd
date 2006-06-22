\name{spec.car}
\alias{spec.car}
\title{Estimate Spectral Density of a Time Series from CAR Fit}
\description{ uses the existing CAR fit) and return (and by default plots) the spectral density of the fitted model.
}
\usage{
spec.car(x, n.freq, plot = TRUE, na.action = na.fail, ...)
}
\arguments{
  \item{x}{ the
    result of a fit by \code{\link{car}}. }
  \item{n.freq}{number of frequency}
  \item{plot}{Plot the periodogram? }
  \item{na.action}{ \code{NA} action function.}
  \item{\dots}{Graphical arguments passed to \code{\link{plot.spec}}. }
}

\value{
    An object of class \code{"spec.car"}.
  The result is returned invisibly if \code{plot} is true.
}
\references{
Belcher, J. and Hampton, J. S. and Tunnicliffe Wilson,
  G. (1994). Parameterization of continuous time autoregressive models
  for irregularly sampled time series data. \emph{Journal of the Royal
    Statistical Society, Series B, Methodological},\bold{56},141--155

Jones, Richard H. (1981). Fitting a continuous time autoregression to
discrete data. \emph{Applied Time Series Analysis II}, 651--682
  Wang, Zhu (2004). \emph{The Application of the Kalman Filter to Nonstationary Time Series through Time Deformation}. PhD thesis, Southern Methodist University

}

\author{
  G. Tunnicliffe Wilson and Zhu Wang
}
\note{The multivariate case is not yet implemented.}

\seealso{\code{\link{car}}}
\examples{
\dontrun{
data(V22174)
(fit <- car(V22174,scale=0.2,order=7))
spec.car(fit)

data(asth)
(fit <- car(asth,scale=0.25,order=4))
spec.car(fit)
}}
\keyword{ ts }
