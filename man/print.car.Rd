\name{print.car}
\alias{print.car}
\title{ Print the Main Results of CAR Fits}
\description{
 Print summary results of CAR fits
}
\usage{
print.car(x, digits = max(3, getOption("digits") - 3), ...)
}

\arguments{
  \item{x}{ a fitted time-series CAR model}
  \item{digits}{ round error option}
  \item{\dots}{further arguments to be passed to particular methods  }
}

\value{
A list of main results from \code{car}.
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
\seealso{\code{\link{car}}}
\examples{
\dontrun{
data(V22174)
(fit <- car(V22174,scale=0.2,order=7))
print(fit)
}}
\keyword{ ts }
