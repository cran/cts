\name{plot.spec.car}
\alias{plot.spec.car}

\title{Plotting Spectral Densities }
\description{
 Internal function used in spectrum.car only. Not called by user.
 Plotting method for objects of class \code{"spectrum.car"}.}
\usage{
plot.spec.car(x, add = FALSE, ci = 0.95, log = "dB", xlab = "frequency",
ylab = NULL, type = "l", main = NULL, sub = NULL,...)
}

\arguments{
    \item{x}{an object of class \code{"spectrum.car"}.}
  \item{add}{logical.  If \code{TRUE}, add to already existing plot.}
  \item{ci}{Coverage probability for confidence interval.  Plotting of
    the confidence bar is omitted unless \code{ci} is strictly
   positive.}
  \item{log}{If \code{"dB"}, plot on log10 (decibel) scale (as S-PLUS),
    otherwise use conventional log scale or linear scale.  Logical
    values are also accepted.  The default is \code{"yes"} unless
    \code{options(ts.S.compat = TRUE)} has been set, when it is
    \code{"dB"}.}
  \item{xlab}{the x label of the plot.}
  \item{ylab}{the y label of the plot.}
  \item{type}{the type of plot to be drawn, defaults to lines.}
  \item{main}{overall title for the plot.}
  \item{sub}{a sub title for the plot.}
  \item{\dots}{further graphical parameters.}
}
\value{plot of spectral density from continuous time autoregressive model}
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
\seealso{ \code{\link{spectrum.car}}}

\keyword{ hplot }
\keyword{ ts }
