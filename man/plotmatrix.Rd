% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotmatrix.R
\name{plotmatrix}
\alias{plotmatrix}
\title{Plot a Matrix as a Table with Colored Cells}
\usage{
plotmatrix(
  mvalue,
  mtext = mvalue,
  ctitle = "Columns",
  rtitle = "Rows",
  vcex = 1,
  lcex = 1,
  mcex = 1,
  ctline = 3,
  rtline = 4,
  colorange = c("white", "magenta"),
  mrange = NULL,
  border = NA,
  ...
)
}
\arguments{
\item{mvalue}{Numeric matrix used to generate colors of table.}

\item{mtext}{Character or numeric matrix to display in the table, default \code{mvalue}.}

\item{ctitle}{Character scalar, column title, default "Columns".}

\item{rtitle}{Character scalar, row title, default "Rows".}

\item{vcex}{Numeric scalar, character expansion for the table values, default 1.}

\item{lcex}{Numeric scalar, character expansion for the column and row axis labels,
default 1.}

\item{mcex}{Numeric scalar, character expansion for the column and row axis titles,
default 1.}

\item{ctline}{Numeric scalar, number of lines from upper margin to display column title,
default 3.}

\item{rtline}{Numeric scalar, number of lines from left margin to display row title,
default 4.}

\item{colorange}{Vector of length 2 giving the color extremes to display for the lowest
and highest values in \code{mvalue}, default c("white", "magenta").}

\item{mrange}{Numeric vector of length 2, an explicit value range corresponding to
\code{colorange}.  When set to NULL, the default, the \code{colorange}
corresponds to the range of values in \code{mvalue}.}

\item{border}{Scalar or matrix of colors to be useed for the cell borders, default NA
gives no border.}

\item{...}{Additional parameters to \code{\link[plotrix]{color2D.matplot}}.}
}
\description{
Plot a matrix as a table with colored cells.
}
\details{
Displays a plot with the same number of rectangular cells as there are
  values in the matrix. Each rectangle is colored to represent its
  corresponding value. The rectangles are arranged in the conventional display
  of a matrix with rows beginning at the top and columns at the left.
}
\examples{
m <- matrix(c(6, 5, 7, 20, 30, 5, 5, 10:14), nrow=3,
 dimnames=list(c("a", "b", "c"), c("W", "X", "Y", "Z")))
plotmatrix(m)
plotmatrix(sqrt(m), m)
plotmatrix(m, mrange=c(0, 50))
}
\references{
Based on a method posted by Henrik on 6 September 2013 on stackoverflow
  \href{http://stackoverflow.com/a/18665994/2140956}{[link]}.
}
\seealso{
\code{\link[plotrix]{color2D.matplot}}
}
