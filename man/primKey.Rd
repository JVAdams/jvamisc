% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/primKey.R
\name{primKey}
\alias{primKey}
\title{Primary Key}
\usage{
primKey(df, varz = 1:dim(df)[2])
}
\arguments{
\item{df}{A data frame in which to find the primary key.}

\item{varz}{A numeric or character vector representing the candidate columns or
variable names to consider for membership in the primary key, default
is to include all variables, \code{1:dim(df)[2]}.}
}
\value{
A list with two elements, \code{npk} an integer giving the number
  of variables that make up the primary key, and \code{k} a list of character
  vectors) identifying the variable names that make up the primary key.
}
\description{
Look for "primary keys" of a data frame.  The primary key is a variable
(or combination of variables) that uniquely identify all rows.
}
\examples{
mydat1 <- data.frame(
 group=c("a", "a", "a", "b", "b", "c", "c", "d", "d"),
 id=1:9,
 value=rnorm(9))
mydat2 <- data.frame(
 group=c("a", "a", "a", "b", "b", "c", "c", "d", "d"),
 id=c(1:3, 1:2, 1:2, 1:2),
 value=rnorm(9))
mydat3 <- data.frame(
 group=c("a", "a", "a", "b", "b", "c", "c", "d", "d"),
 name=c("ax", "ax", "ax", "be", "be", "cat", "cat", "dog", "dog"),
 id=c(1:3, 1:2, 1:2, 1:2),
 value=rnorm(9))
# primary key made up of one variable
primKey(mydat1[, c("group", "id")])
# primary key made up of two variables
primKey(mydat2[, c("group", "id")])
# two primary keys, each made up of two variables
primKey(mydat3[, c("group", "name", "id")])
}
