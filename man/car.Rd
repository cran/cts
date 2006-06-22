\name{car}
\alias{car}
\title{Fit Continuous Time AR Models to Irregularly Sampled Time Series}
\description{
  Fit a continuous AR model to an irregularly sampled univariate time series with the Kalman filter
}
\usage{
car(x, y=NULL, scale = 1.5, order = 3, ari= TRUE, phi = rep(0, order),
vri = FALSE, vr = 0, pfi = "MAPS", ccv = "CTES", lpv = TRUE,
scc = TRUE, n.ahead = 10, nit = 40, opm = 1, rgm = 1, req = 0.5,
con = 1e-05, rpe = 1, ivl = 0.01, fac = 10, stl = 1e-05,
sml = 100, gtl = 1e+05, kst = TRUE, fct = TRUE, fty=2)
}
\arguments{
  \item{x}{two column data frame or matrix with the first column
    being the sampled time and the second column being the observations at
    the first column; otherwise \code{x} is a numeric vector of sampled time.}
  \item{y}{not used if \code{x} has two columns; otherwise \code{y} is a numeric vector of observations at sampled time \code{x}.}
  \item{scale}{ The kappa value referred to in the paper.}
  \item{order}{order of autoregression.}
  \item{ari}{ \code{ari=TRUE}: parameter starting values follow \code{phi}. \code{ari=FALSE}: they are taken as zero}
  \item{phi}{ parameter starting values used only if \code{ari=TRUE}.}
  \item{vri}{ \code{vri=FALSE}, observation noise not included in the
    model. \code{vri=TRUE}, observation noise included}
  \item{vr}{ 0.5, initial value of observation noise ratio: only if
    \code{vri=TRUE}}
  \item{pfi}{ always use the option pfi="MAPS".}
  \item{ccv}{\code{ccv="CTES"} for constant term estimation. \code{ccv="MNCT"} if mean correction, \code{ccv=NULL} if omitted.}
  \item{lpv}{\code{lpv=TRUE} always use this option.}
  \item{scc}{ \code{scc=TRUE} always use this option. }
  \item{n.ahead}{number of steps ahead at which to predict.}
  \item{nit}{number of iteations. }
  \item{opm}{\code{opm=1} always use this.}
  \item{rgm}{\code{rgm=1} always use this.}
  \item{req}{root equality switch value.}
  \item{con}{convergence criterion.}
  \item{rpe}{relative size of parameter perturbations.}
  \item{ivl}{initial value of step size constraint parameter.}
  \item{fac}{step size constraint modification parameter. This value may
  be setup to \code{fac=5} for better convergency.}
  \item{stl}{typical smallest step size parameter.}
  \item{sml}{typical small step size parametrr.}
  \item{gtl}{typical greatest step size parameter. }
  \item{kst}{\code{kst=TRUE} to save estimated states. }
  \item{fct}{\code{fct=TRUE} to use all time series to fit the model. }
  \item{fty}{\code{fty=1} forecast past the end. \code{fty=2} forecast
    last L-steps. \code{fty=3} forecast last L-steps updated (filtering)types.}
}
\details{
  See references.
}
\value{
  A list of class \code{"car"} with the following elements:
  \item{n.used}{The number of observations of \code{ser} used in fitting}
  \item{order }{The order of the fitted model. This is chosen by the user.}
  \item{np }{The number of parameters estimated. This may include the mean and the observation noise ratio.}
  \item{scale}{The kappa value referred to in the paper.}
  \item{vr}{The estimated observation noise ratio.}
  \item{sigma2}{The estimated innovation variance.}
  \item{phi}{The estimated reparameterized autoregressive parameters.}
  \item{x.mean}{The estimated mean of the series used in fitting and for use in prediction.}
  \item{b}{All estimated parameters, which include \code{phi}, and possibly \code{x.mean} and \code{vr}.}
  \item{delb}{The estimated standard error of \code{b}}
  \item{essp}{The estimated correlation matrix of \code{b}}
  \item{ecov}{The estimated covariance matrix of \code{phi}. See also
    \code{\link{aic}}}
  \item{rootr}{The real part of roots of \code{phi}. See also \code{\link{aic}}}
  \item{rooti}{The imaginary part of roots of \code{phi}. See also
    \code{\link{aic}}}
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
  \item{predict}{Predictions for the series which has been used to fit the model.}
  \item{predict.var}{Prediction variance of \code{predict}}
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
\seealso{\code{\link{aic}} for model selection}
\examples{
\dontrun{
data(V22174)
car(V22174,scale=0.2,order=7)

data(asth)
car(asth,scale=0.25,order=4)
}}
\keyword{ts}
