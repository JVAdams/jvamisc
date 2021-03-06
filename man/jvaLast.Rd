% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jvaLast.R
\name{jvaLast}
\alias{jvaLast}
\title{Shutdown}
\usage{
jvaLast(file = ".Rhistory", nlines = 10000)
}
\arguments{
\item{file}{Character scalar, file in which to save the commands history relative to
current working directory, default ".Rhistory".}

\item{nlines}{Integer scalar, number of lines that saved to commands history,
default 10,000.}
}
\description{
One function with all the commands I typically want run at the end of an
R session.
}
\seealso{
\code{\link{savehistory}}, \code{\link{Sys.setenv}}.
}
