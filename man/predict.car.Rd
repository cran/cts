\name{predict.car}
\alias{predict.car}
\title{Forecast from CAR fits}
\description{
Forecast from models fitted by \code{\link{car}} 
}
\usage{
predict.car(object, se.fit = TRUE, digits = max(3, getOption("digits") - 3), ...)
}
\arguments{
  \item{object}{the result of a \code{car} fit.}
  \item{se.fit}{ Logical: should standard errors of prediction be returned?}
  \item{digits}{ return value digits }
  \item{\dots}{arguments passed to or from other methods. }
}
\details{
The Kalman filter forecasting algorithm is used.
}
\value{
 A time series of predictions, or if \code{se.fit = TRUE}, a list
  with components \code{predict}, the predictions, and \code{se},
  the estimated standard errors.  Both components are time series.
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
\seealso{  \code{\link{car}}}
\examples{
\dontrun{
data(V22174)
(fit <- car(V22174,scale=0.2,order=7,n.ahead=10))
predict(fit)

data(asth)
(fit <- car(asth,scale=0.25,order=4,n.ahead=10))
predict(fit)
}}
\keyword{ ts }
