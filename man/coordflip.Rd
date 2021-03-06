% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coordflip.R
\name{coordflip}
\alias{coordflip}
\title{Flip Coordinates}
\usage{
coordflip(pts, seg)
}
\arguments{
\item{pts}{A numeric matrix with two columns of x and y coordinates to be rotated.}

\item{seg}{A numeric matrix of dimension 2 x 2 giving two points which define
the line over which the coordinates will be flipped.}
}
\value{
A numeric matrix with same dimension as \code{pts} with
  the flipped x and y coordinates.
}
\description{
Flip (or reflect) coordinates across any given line.
}
\examples{
# starting coordinates
test <- matrix(c(0, 4, 1, 0, 2, 3), ncol=2,
 dimnames=list(LETTERS[1:3], NULL))
coordplot(test)
# flip the coordinates across the line defined by the first two points
ftest <- coordflip(test, test[1:2, ])
coordplot(ftest)
}
\references{
Based on a method posted by Il-Bhima on 22 July 2010 on stackoverflow
  \href{http://stackoverflow.com/a/3307181/2140956}{[link]}.
}
\seealso{
\code{\link[jvamisc]{coordplot}}, \code{\link[jvamisc]{coordmove}},
  \code{\link[jvamisc]{coordturn}}, \code{\link[jvamisc]{coordtri}}.
}
