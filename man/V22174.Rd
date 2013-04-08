\name{V22174}
\alias{V22174}
\docType{data}
\title{ Measurments of Relative Aboundance}
\description{
  Measurments of relative aboundance of an xxygen isotope in an ocean core.
}
\usage{data(V22174)}
\format{
  The format is:
 num [1:164, 1:2]  6.13  8.39 10.64 12.90 15.16 ...
}

\source{
Belcher, J. and Hampton, J. S. and Tunnicliffe Wilson,
  G. (1994). Parameterization of continuous time autoregressive models
  for irregularly sampled time series data. \emph{Journal of the Royal
    Statistical Society, Series B, Methodological},\bold{56},141--155
}
\references{
 Wang, Zhu (2013). cts: An R Package for Continuous Time Autoregressive Models via Kalman Filter. \emph{Journal of Statistical Software}, \bold{Vol. 53(5)}, 1--19. \url{http://www.jstatsoft.org/v53/i05}
}

\examples{
data(V22174)
str(V22174) ; plot(V22174)
}
\keyword{datasets}
