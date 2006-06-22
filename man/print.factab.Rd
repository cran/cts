\name{print.factab}
\alias{print.factab}
\title{Print the Results of Characteristic Roots and System Frequency}
\description{
Print summary results of calculated characteristic roots and system
frequency from the estimated reparameterized coefficients of CAR fits.
}
\usage{
print.factab(x,digits = max(3, getOption("digits") - 3), ...)
}

\arguments{
  \item{x}{calculated from factab}
  \item{digits}{round error option}
  \item{\dots}{further arguments to be passed to particular methods  }
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
  Wang, Zhu (2004). \emph{The Application of the Kalman Filter to Nonstationary Time Series through Time Deformation}. PhD thesis, Southern Methodist University

}

\author{
  G. Tunnicliffe Wilson and Zhu Wang
}
\seealso{ \code{\link{car}} and \code{\link{factab}}}
\examples{
\dontrun{
data(asth)
(fit <- car(asth,scale=0.25,order=4))
print.factab(factab(fit))
}}
\keyword{ ts }
