% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mytable.R
\name{mytable}
\alias{mytable}
\title{Cross Tabulation and Table Creation}
\usage{
mytable(...)
}
\arguments{
\item{...}{Arguments provided to \code{\link{table}}.}
}
\value{
An array of integer values of class "table".
}
\description{
Build a contingency table of the counts at each combination of factor levels,
incorporating missing values by default.
}
\examples{
mytable(c(1, 1, 1, 2, NA, 3, 4, 1, 10, 3))

}
