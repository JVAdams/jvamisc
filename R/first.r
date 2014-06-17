#' Identify the First Elements of Series of Repeated Values
#'
#' Identify the first elements of series of repeated values.
#' @param x 			A vector whose values will be explored for series of repeated values, can be character, numeric, or factor.
#' @return 				An integer vector the same length as \code{x}, with a 1 for every element that is different than the one before it, 
#' and a 0 for every element that is the same as the one before it.
#' @export
#' @examples 
#' first(c(1, 2, 1, 2, 2, 1, 1, 3))
first <- function(x) {
    l <- length(x)
    c(1, 1-(x[-1]==x[-l]))
    }
