\name{car_control}
\alias{car_control}
\title{ Parameters for Predict and Numerical Optimization in Kalman Filter }
\description{
  Setup in the predict and numerical optimization in the Kalman filter algorithm.
}
\usage{
car_control(fty=1, n.ahead=10, trace=FALSE, ari=TRUE, vri=FALSE, vr=0, pfi="MAPS",
ccv="CTES", lpv=TRUE, scc=TRUE,  nit=40, opm=1, rgm=1, req=0.5, con=1.0e-5, rpe=1.0, 
ivl=1.0e-2, fac=1.0e1, stl=1.0e-5, sml=1.0e2, gtl=1.0e5, kst=TRUE, fct=TRUE)
}
\arguments{
  \item{fty}{\code{fty=1} forecast past the end. \code{fty=2} forecast
    last L-steps. \code{fty=3} forecast last L-steps updated (filtering)types. See also \code{fct}. If \code{fct=TRUE}, all time series is used to fit the model for \code{fty=1 or 3}. If \code{fty=2}, only the first (length of time - n.ahead) is used to fit the model. Thus, only the first (length of time - n.head) prediction values are the same for \code{fty=1 or 3} vs \code{fty=2}. See also \code{pre2, prv2}.}
  \item{n.ahead}{number of steps ahead at which to predict.}
  \item{trace}{ a logical value triggering printout of information during
                the fitting process, and major results for the fitted model.}
  \item{ari}{ \code{ari=TRUE}: parameter starting values. \code{ari=FALSE}: they are taken as zero. This is obsolete.}
  \item{vri}{ \code{vri=FALSE}, observation noise not included in the
    model. \code{vri=TRUE}, observation noise included}
  \item{vr}{ 0.5, initial value of observation noise ratio: only if
    \code{vri=TRUE}}
  \item{pfi}{ always use the option pfi="MAPS".}
  \item{ccv}{\code{ccv="CTES"} for constant term estimation. \code{ccv="MNCT"} if mean correction, \code{ccv=NULL} if omitted.}
  \item{lpv}{\code{lpv=TRUE} always use this option.}
  \item{scc}{ \code{scc=TRUE} always use this option. }
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
}
\details{

  Objects returned by this function specify predict and numerical optimization parameters of the Kalman filter algorithms implemented in \code{\link{car}},
  (via the \code{ctrl} argument).

}
\value{
  An object of class \code{car_control}, a list.
}
\seealso{\code{\link{car}} for the usage}
\keyword{misc}

