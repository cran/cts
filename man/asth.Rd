\name{asth}
\alias{asth}
\docType{data}
\title{Measurements of The Lung Function}
\description{
Measurements of the lung function of an asthma patient.
}
\usage{data(asth)}
\format{
  The format is:
 num [1:209, 1:2] 8 10 12 14 16 18 20 22 32 34 ...
}
\details{
These are made by the patients mostly at 2h time intervals but with
irregular gaps in this record.
}
\source{
Belcher, J. and Hampton, J. S. and Tunnicliffe Wilson,
  G. (1994). Parameterization of continuous time autoregressive models
  for irregularly sampled time series data. \emph{Journal of the Royal
    Statistical Society, Series B, Methodological},\bold{56},141--155
}

\examples{
\dontrun{
data(asth)
str(asth) ; plot(asth)
}}
\keyword{datasets}
