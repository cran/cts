\name{plotSpecLs}
\alias{plotSpecLs}
\title{Plotting Lomb-Scargle Periodogram}
\description{
  Plotting method for objects of class \code{"spec.ls"}.
}
\usage{
plotSpecLs(x, add = FALSE, ci = 0.95, log = c("yes", "dB", "no"), xlab
= "frequency", ylab = NULL, type = "l", main = NULL, sub = NULL, ...)
}
\arguments{
    \item{x}{an object of class \code{"spec.car"}.}
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
  \item{\dots}{Further graphical parameters.}
}

\seealso{ \code{\link{spec.ls} }}
\keyword{ hplot }
\keyword{ ts }
