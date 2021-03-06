% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CI.R
\name{CI}
\alias{CI}
\title{Confidence Interval of Mean}
\usage{
CI(x, alpha = 0.05, keepMean = TRUE, limits = TRUE, prefix = "",
  na.rm = FALSE)
}
\arguments{
\item{x}{Numeric vector of sample observations.}

\item{alpha}{Numeric scalar denoting significance level, default 0.05.}

\item{keepMean}{Logical scalar indicating if the mean should be returned with the
confidence interval/limits, default TRUE.}

\item{limits}{Logical scalar indicating if the limits should be returned
(otherwise a single interval is returned), default TRUE.}

\item{prefix}{Character scalar to be used in assigning names to the returned value,
default "".}

\item{na.rm}{Logical scalar indicating if missing values should be removed before
calculations, default FALSE.}
}
\value{
A named vector of the mean (if \code{keepMean=TRUE}) and the
  (1 - \code{alpha})*100% confidence limits (if \code{limits=TRUE}) or
  interval (if \code{limits=FALSE}).
  Names are the \code{prefix} concatenated to "mean", "lo", and "hi".
}
\description{
Calculate the confidence interval or limits of the mean using the
t distribution.
}
\examples{
CI(1:10)
}
