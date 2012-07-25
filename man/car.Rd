\name{car}
\alias{car}
\alias{print.car}
\alias{summary.car}
\alias{plot.car}
\alias{predict.car}
\alias{spectrum}
\alias{spectrum.car}
\alias{tsdiag}
\alias{tsdiag.car}
\alias{AIC.car}
\alias{kalsmo}
\alias{kalsmo.car}
\title{Fit Continuous Time AR Models to Irregularly Sampled Time Series}
\description{
  Fit a continuous AR model to an irregularly sampled univariate time series with the Kalman filter
}
\usage{
car(x, y=NULL, scale = 1.5, order = 3, ctrl=car_control())
\method{print}{car}(x, digits = 3, ...)
\method{summary}{car}(object, ...)
\method{plot}{car}(x, type=c("spec", "pred", "diag"),...) 
\method{predict}{car}(object, se.fit = TRUE, digits = 3, plot.it=TRUE,...)
\method{spectrum}{car}(object, frmult=1, n.freq, plot.it = TRUE, na.action = na.fail, ...)
\method{AIC}{car}(object, ..., k=NULL)
\method{tsdiag}{car}(object, gof.lag = 10, ...)
\method{kalsmo}{car}(object)
}
\arguments{
  \item{x}{two column data frame or matrix with the first column
    being the sampled time and the second column being the observations at
    the first column; otherwise \code{x} is a numeric vector of sampled time. It can be a \code{car} object for S3 methods}
  \item{y}{not used if \code{x} has two columns; otherwise \code{y} is a numeric vector of observations at sampled time \code{x}.}
  \item{scale}{ The kappa value referred to in the paper.}
  \item{order}{order of autoregression.}
  \item{ctrl}{ control parameters used in predict and numerical optimization. }
  \item{object}{ object of class \code{car} }
  \item{type}{ in \code{plot} a character indicating the type of plot. type="spec", call \code{spec};
                 type="pred", call \code{predict};
                 type="diag", call \code{diag}}
  \item{se.fit}{ Logical: should standard errors of prediction be returned?}
  \item{digits}{ return value digits }
  \item{plot.it}{ Logical: plot the forecast values? }
  \item{gof.lag}{the maximum number of lags for a Portmanteau
    goodness-of-fit test}
  \item{frmult}{ numerical value, can be used to multiply the frequency range}
  \item{n.freq}{number of frequency}
  \item{k}{penalty, not used}
  \item{na.action}{ \code{NA} action function.}
  \item{\dots}{further arguments to be passed to particular methods}
}
\details{
\code{spectrum} returns (and by default plots) the spectral density of the fitted model.

\code{tsdiag} is a generic diagnostic function for continuous AR model. It will generally plot the residuals,
  often standadized, the autocorrelation function of the residuals, and
  the p-values of a Portmanteau test for all lags up to \code{gof.lag}.
  The method for \code{\link{car}} object plots residuals scaled by the estimate of their (individual) variance, and use the Ljung--Box version of the portmanteau test.

\code{AIC} For continuous CAR model selection, t-statistic and AIC are calculated
based on reparameterized coefficients \code{phi} and covariance matrix
\code{ecov}. From the t-statistic, the final model is chosen such that
if the true model \code{order} is less than the large value used for
model estimation then for \code{i > order} the deviations of the
estimated parameters \code{phi} from their true value of 0 will be
small. From the AIC, the final model is chosen based on the smallest AIC
value. A table with t-statistic and AIC for the corresponding model order.

\code{factab} calculate characteristic roots and system frequency from the estimated
reparameterized coefficients of CAR fits.

\code{smooth} computes components corresponding to the diagonal transition matrix with the Kalman smoother. This may not be stable for some data due to numerical inversion of matrix.

}
\value{
  A list of class \code{"car"} with the following elements:
  \item{n.used}{The number of observations of \code{ser} used in fitting}
  \item{order }{The order of the fitted model. This is chosen by the user.}
  \item{np }{The number of parameters estimated. This may include the mean and the observation noise ratio.}
  \item{scale}{The kappa value referred to in the paper of Belcher et al.}
  \item{vri}{If vri=1, estimate the observation noise ratio.}
  \item{vr}{The estimated observation noise ratio.}
  \item{sigma2}{The estimated innovation variance.}
  \item{phi}{The estimated reparameterized autoregressive parameters.}
  \item{x.mean}{The estimated mean of the series used in fitting and for use in prediction.}
  \item{b}{All estimated parameters, which include \code{phi}, and possibly \code{x.mean} and \code{vr}.}
  \item{delb}{The estimated standard error of \code{b}}
  \item{essp}{The estimated correlation matrix of \code{b}}
  \item{ecov}{The estimated covariance matrix of \code{phi}. See also
    \code{\link{AIC}}}
  \item{rootr}{The real part of roots of \code{phi}. See also \code{\link{AIC}}}
  \item{rooti}{The imaginary part of roots of \code{phi}. See also
    \code{\link{AIC}}}
  \item{tim}{The numeric vector of sampled time.}
  \item{ser}{The numeric vector of observations at sampled time
    \code{tim}.}
  \item{filser}{The filtered time series with the Kalman filter.}
  \item{filvar}{The estimated variance of Kalman filtered time series
    \code{filser}}
  \item{sser}{The smoothed time series with the Kalman smoother.}
  \item{svar}{The estimated variance of smoothed time series
    \code{sser}}
  \item{stdred}{The standardized residuals from the fitted model.}
  \item{pretime}{Time of predictions.}
  \item{pred}{Predictions for the \code{pretime}.}
  \item{prv}{Prediction variance of \code{pred}}
  \item{fty}{Forecast type}
  \item{tnit}{Numeric vector: iteration}
  \item{ss}{Numeric vector: sum of squares for each \code{tnit}}
  \item{bit}{Matrix with rows for \code{tnit} and columns for parameter estimates}
  \item{aic}{AIC value for the fitted model}
  \item{bic}{BIC value for the fitted model}
}
\references{
  Belcher, J. and Hampton, J. S. and Tunnicliffe Wilson,
  G. (1994). Parameterization of continuous time autoregressive models
  for irregularly sampled time series data. \emph{Journal of the Royal
    Statistical Society, Series B, Methodological},\bold{56},141--155
  
  Jones, Richard H. (1981). Fitting a continuous time autoregression to
  discrete data. \emph{Applied Time Series Analysis II}, 651--682

  Wang, Zhu (2004). \emph{The Application of the Kalman Filter to Nonstationary Time Series through Time Deformation}. PhD thesis, Southern Methodist University

  Wang, Zhu and Woodward, W. A. and Gray, H. L. (2009). The Application of the Kalman Filter to Nonstationary Time Series through Time Deformation. \emph{Journal of Time Series Analysis}, \bold{30(5)}, 559-574.

  Wang, Zhu (2011). cts: An R Package for Continuous Time Autoregressive Models via Kalman Filter. Under review.
}

\author{
  G. Tunnicliffe Wilson and Zhu Wang
}
\seealso{\code{\link{car_control}} for predict and numerical optimization parameters, and \code{\link{AIC}} for model selection}
\examples{
data(V22174)
fit <- car(V22174,scale=0.2,order=7, ctrl=car_control(trace=TRUE))
summary(fit)
spectrum(fit)
tsdiag(fit)
AIC(fit)
factab(fit)
data(asth)
fit <- car(asth,scale=0.25,order=4, ctrl=car_control(n.ahead=10))
kalsmo(fit)
}
\keyword{ts}
