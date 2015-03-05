#' Identify the Last Elements of Series of Repeated Values
#'
#' Identify the last elements of series of repeated values.
#' @param x
#'   A vector whose values will be explored for series of repeated values,
#'   can be character, numeric, or factor.
#' @return
#'   An integer vector the same length as \code{x}, with a 1 for every element
#'   that is different than the one after it, and a 0 for every element
#'   that is the same as the one after it.
#' @export
#' @examples
#' last(c(1, 2, 1, 2, 2, 1, 1, 3))

last <- function(x) {
  y <- rev(x)
  l <- length(y)
  rev(c(1, 1-(y[-1]==y[-l])))
}
