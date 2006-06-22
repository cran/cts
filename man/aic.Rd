\name{aic}
\alias{aic}

\title{Model Selection Statistics }
\description{
Compute t-statistic and AIC
}
\usage{
aic(object)
}

\arguments{
  \item{object}{a fitted time-series CAR model}
}
\details{
For continuous CAR model selection, t-statistic and AIC are calculated
based on reparameterized coefficients \code{phi} and covariance matrix
\code{ecov}. From the t-statistic, the final model is chosen such that
if the true model \code{order} is less than the large value used for
model estimation then for \code{i > order} the deviations of the
estimated parameters \code{phi} from their true value of 0 will be
small. From the AIC, the final model is chosen based on the smallest AIC
value.
}
\value{
A table with t-statistic and AIC for the corresponding model order.
}
\references{
  Belcher, J. and Hampton, J. S. and Tunnicliffe Wilson,
  G. (1994). Parameterization of continuous time autoregressive models
  for irregularly sampled time series data. \emph{Journal of the Royal
    Statistical Society, Series B, Methodological},\bold{56},141--155

}
\author{
  G. Tunnicliffe Wilson and Zhu Wang
}
\note{This is not "true" AIC }

\seealso{\code{\link{car}}}
\examples{
\dontrun{
data(V22174)
(fit <- car(V22174,scale=0.2,order=14))
aic(fit)
}}
\keyword{ ts }
