% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kstest.R
\name{kstest}
\alias{kstest}
\title{Kolmogorov-Smirnov Test}
\usage{
kstest(
  x,
  y,
  ids = c("X", "Y"),
  leg.loc = "right",
  txt.loc = "left",
  cexall = 0.8,
  cexmain = 0.8
)
}
\arguments{
\item{x}{A numeric vector representing the values from the first distribution.}

\item{y}{A numeric vector representing the values from the second distribution.}

\item{ids}{A character vector of length 2 assigning names to the two distributions.}

\item{leg.loc}{A character scalar denoting the location of the legend, specified as one
of the following: "bottomright", "bottom", "bottomleft", "left", "topleft",
"top", "topright", "right" (the default) and "center".}

\item{txt.loc}{A character scalar denoting the location of additional text, either "left"
(the default) or "right".}

\item{cexall}{A numeric scalar giving the amount by which plotting text and symbols
should be magnified, default 0.8.}

\item{cexmain}{A numeric scalar giving the amount by which the main text heading
should be magnified, default 0.8.}
}
\value{
A two panel plot of the smoothed probability and cumulative distributions
  is generated with a vertical line at the maximum distance and a summary
  of the Kolmogorov-Smirnov test annotated on the plot.
}
\description{
Conduct a two-sample Kolmogorov-Smirnov test and output detailed
information on the results, including a plot of the distributions.
}
\examples{
myX <- rnorm(50, mean=3)
myY <- rnorm(20, mean=4)
kstest(x=myX, y=myY)
}
\seealso{
\code{\link{ks.test}}.
}
