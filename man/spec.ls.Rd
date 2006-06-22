\name{spec.ls}
\alias{spec.ls}
\title{ Estimate Spectral Density of an Irregularly Sampled Time Series by a Smoothed
    Periodogram}
\description{
The most commonly used method of computing the spectrum on unevenly
spaced time series is periodogram analysis, see Lomb (1975) and Scargle
(1982). The Lomb-Scargle method for unevenly spaced data is known to be
a powerful tool to find, and test significance of, weak perriodic
signals. The Lomb-Scargle periodogram possesses the same statistical
properties of standard power spectra.
}
\usage{
spec.ls(x, y=NULL, spans = NULL, kernel = NULL, taper = 0.1, pad = 0,
fast = TRUE, type = "lomb",demean = FALSE, detrend = TRUE, plot = TRUE,
na.action = na.fail, ...)
}
\arguments{
  \item{x}{two column data frame or matrix with the first column
    being the sampled time and the second column being the observations at
    the first column; otherwise \code{x} is a numeric vector of sampled time.}
  \item{y}{not used if \code{x} has two columns; otherwise \code{y} is a numeric vector of observations at sampled time \code{x}.the time at which \code{x} is observed}
  \item{spans}{vector of odd integers giving the widths of modified
    Daniell smoothers to be used to smooth the periodogram.}
  \item{kernel}{alternatively, a kernel smoother of class
    \code{"tskernel"}.}
  \item{taper}{proportion of data to taper.  A split cosine bell taper
    is applied to this proportion of the data at the beginning and
    end of the series.}
  \item{pad}{proportion of data to pad. Zeros are added to the end of
    the series to increase its length by the proportion \code{pad}.}
  \item{fast}{logical; if \code{TRUE}, pad the series to a highly composite
    length.}
  \item{type}{Lomb-Scargle spectrum of Fourier transformation spectrum}
  \item{demean}{logical. If \code{TRUE}, subtract the mean of the
    series.}
  \item{detrend}{logical. If \code{TRUE}, remove a linear trend from
    the series. This will also remove the mean.}
  \item{plot}{plot the periodogram?}
  \item{na.action}{\code{NA} action function.}
  \item{\dots}{graphical arguments passed to \code{plot.spec.ls}.}
}

\details{
  The raw Lomb-Scargle periodogram for irregularly sampled time series is not a consistent estimator of the spectral density,
  but adjacent values are asymptotically independent. Hence a consistent
  estimator can be derived by smoothing the raw periodogram, assuming that
  the spectral density is smooth.

  The series will be automatically padded with zeros until the series
  length is a highly composite number in order to help the Fast Fourier
  Transform. This is controlled by the \code{fast} and not the \code{pad}
  argument.

  The periodogram at zero is in theory zero as the mean of the series
  is removed (but this may be affected by tapering): it is replaced by
  an interpolation of adjacent values during smoothing, and no value
  is returned for that frequency.
}
\value{
  A list object of class \code{"spec.ls"} with the following additional components:
  \item{kernel}{The \code{kernel} argument, or the kernel constructed
    from \code{spans}.}
  \item{df}{The distribution of the spectral density estimate can be
    approximated by a chi square distribution with \code{df} degrees
    of freedom.}
  \item{bandwidth}{The equivalent bandwidth of the kernel smoother as
    defined by Bloomfield (1976, page 201).}
  \item{taper}{The value of the \code{taper} argument.}
  \item{pad}{The value of the \code{pad} argument.}
  \item{detrend}{The value of the \code{detrend} argument.}
  \item{demean}{The value of the \code{demean} argument.}

  The result is returned invisibly if \code{plot} is true.
}
\references{
Bloomfield, P. (1976) \emph{Fourier Analysis of Time Series: An
  Introduction.}Wiley.

Lomb, N. R. (1976) Least-squares frequency-analysis of unequally spaced data.
  \emph{Astrophysics and Space Science},
  \bold{39},447-462

W. H. Press and S. A. Teukolsky and W. T. Vetterling and B.P. Flannery (1992)
\emph{Numerical Recipes in C: The Art of Scientific Computing.}, Cambridge University Press,
second edition.

Scargle, J.D. (1982) Studies in astronomical time series analysis II. Statistical aspects of spectral analysis of unevenly spaced data,
\emph{The Astrophysical Journal},
\bold{263}, volume 2, 835-853.
}

\author{Lomb-Scargle periodogram by Zhu Wang
 }
\note{This is 'slow' program and a fast program may use FFT, see (Press
  et al, 1992) }

\seealso{ \code{\link{spec.taper}},
  \code{\link{plot.spec.ls}}, \code{\link{fft}}}

\examples{
\dontrun{
data(V22174)
spec.ls(V22174)

data(asth)
spec.ls(asth)
}}
\keyword{ ts }
