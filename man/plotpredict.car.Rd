\name{plotpredict.car}
\alias{plotpredict.car}
\title{Plot of forecast from CAR fits}
\description{
PLot of forecast from models fitted by \code{\link{car}} 
}
\usage{
plotpredict.car(object, xlab = "time", ylab = NULL, type = "l", main = NULL, sub = NULL,...)
}
\arguments{
  \item{object}{the result of a \code{car} fit.}.
  \item{xlab}{the x label of the plot.}
  \item{ylab}{the y label of the plot.}
  \item{type}{the type of plot to be drawn, defaults to lines.}
  \item{main}{overall title for the plot.}
  \item{sub}{a sub title for the plot.}
  \item{\dots}{further graphical parameters.}
}
\details{
See predict.car.
}
\value{
 A plot of original continuous time series along with predictions and
 95\% confidence intervals}
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
data(asth)
(fit <- car(asth,scale=0.25,order=4,n.ahead=10))
plotpredict.car(predict(fit)
}}
\keyword{ ts }
