\name{factab}
\alias{factab}
\title{Calculate Characteristic Roots and System Frequency}
\description{
Calculate characteristic roots and system frequency from the estimated
reparameterized coefficients of CAR fits.
}
\usage{
factab(object)
}

\arguments{
  \item{object}{a fitted time-series CAR model  }
}

\value{
A table with characteristic roots and frequencies for the corresponding model fit.
}
\references{
  Belcher, J. and Hampton, J. S. and Tunnicliffe Wilson,
  G. (1994). Parameterization of continuous time autoregressive models
  for irregularly sampled time series data. \emph{Journal of the Royal
    Statistical Society, Series B, Methodological},\bold{56},141--155

Jones, Richard H. (1981). Fitting a continuous time autoregression to
discrete data. \emph{Applied Time Series Analysis II}, 651--682

  Wang, Zhu(2004). \emph{The Application of the Kalman Filter to Nonstationary Time Series through Time Deformation}. PhD thesis, Southern Methodist University

 Wang, Zhu (2013). cts: An R Package for Continuous Time Autoregressive Models via Kalman Filter. \emph{Journal of Statistical Software}, \bold{Vol. 53(5)}, 1--19. \url{http://www.jstatsoft.org/v53/i05}
}

\author{
  G. Tunnicliffe Wilson and Zhu Wang
}
\seealso{ \code{\link{car}} and \code{\link{kalsmo}}}
\examples{
data(asth)
(fit <- car(asth,scale=0.25,order=4))
factab(fit)
}
\keyword{ ts }
