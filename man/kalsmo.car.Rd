\name{kalsmo.car}
\alias{kalsmo.car}
\title{ Compute Components with the Kalman Smoother}
\description{
Compute Components corresponding to the diagonal transition matrix with the Kalman Smoother.
}
\usage{
kalsmo.car(object)
}
\arguments{
  \item{object}{a fitted time-series CAR model  }
}
\details{
 See references.
}
\value{
Compute components corresponing to all roots of the chracteristic
equation of the CAR model.
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
\note{\code{\link{car}} can return non-diagonal components}
\seealso{ \code{\link{car}} and \code{\link{factab}}}

\examples{
\dontrun{
data(asth)
(fit <- car(asth,scale=0.25,order=4))
kalsmo.car(fit)
}}
\keyword{ ts }
