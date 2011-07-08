\name{kalsmoComp}
\alias{kalsmoComp}
\title{Estimate Componenents with the Kalman Smoother }
\description{
Estimate unobserved components with the Kalman smoother to a fitted CAR model.
}
\usage{
kalsmoComp(x, comp = NULL, plot.it = TRUE, xlab= "time", ylab="",na.action = na.fail, ...)
}
\arguments{
  \item{x}{ the result of estimated components by \code{\link{kalsmo}}. }
  \item{comp}{ a numeric vector from which components are estimaed }
  \item{plot.it}{ plot the component? }
  \item{xlab}{ name of xlab}
  \item{ylab}{ name of ylab}
  \item{na.action}{ how to handle NAs? }
  \item{\dots}{further graphical parameters. }
}

\value{
A component is computed from the estimated components for each root of the
characteristic equation.
}
\references{
Belcher, J. and Hampton, J. S. and Tunnicliffe Wilson,
  G. (1994). Parameterization of continuous time autoregressive models
  for irregularly sampled time series data. \emph{Journal of the Royal
    Statistical Society, Series B, Methodological},\bold{56},141--155

  Wang, Zhu(2004). \emph{The Application of the Kalman Filter to Nonstationary Time Series through Time Deformation}. PhD thesis, Southern Methodist University

}

\author{
  Zhu Wang
}

\seealso{ \code{\link{kalsmo}}}

\examples{
data(asth)
kalsmoComp(kalsmo(car(asth,scale=0.25,order=4)),c(2,3))
}
\keyword{ ts }
