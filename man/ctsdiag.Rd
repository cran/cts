\name{ctsdiag}
\alias{ctsdiag}
\alias{tsdiag.car}

\title{ Diagnostic Plots for Time-Series CAR Fits}
\description{
  A generic function to plot time-series diagnostics.
}
\usage{
ctsdiag(object, gof.lag = 10, ...)
}

\arguments{
  \item{object}{a fitted time-series CAR model}
  \item{gof.lag}{the maximum number of lags for a Portmanteau
    goodness-of-fit test}
  \item{\dots}{further arguments to be passed to particular methods}
  }

\details{
  This is a generic function. It will generally plot the residuals,
  often standadized, the autocorrelation function of the residuals, and
  the p-values of a Portmanteau test for all lags up to \code{gof.lag}.

  The method for \code{\link{car}} object
  plots residuals scaled by the estimate of their (individual) variance,
  and use the Ljung--Box version of the portmanteau test.
}
\value{
 None. Diagnostics are plotted.
}
\references{
  Belcher, J. and Hampton, J. S. and Tunnicliffe Wilson,
  G. (1994). Parameterization of continuous time autoregressive models
  for irregularly sampled time series data. \emph{Journal of the Royal
    Statistical Society, Series B, Methodological},\bold{56},141--155
  
  Jones, Richard H. (1981). Fitting a continuous time autoregression to
  discrete data. \emph{Applied Time Series Analysis II}, 651--682

  Wang, Zhu(2004). \emph{The Application of the Kalman Filter to Nonstationary Time Series through Time Deformation}. PhD thesis, Southern Methodist University

}

\author{
  G. Tunnicliffe Wilson and Zhu Wang
}
\seealso{ \code{\link{car}}}
\examples{
\dontrun{data(V22174)
(fit <- car(V22174,scale=0.2,order=7))
ctsdiag(fit)
}}
\keyword{ ts }
