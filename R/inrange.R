#' In Range
#'
#' Test to see if value falls within specified range.
#' @param x
#'   A numeric vector of values to be tested.
#' @param r
#'   A numeric vector of length 2 specifying the range.
#' @param open
#'   A logical scalar indicating if the range interval is open
#'   (excludes endpoints, TRUE, default) or
#'  closed (includes endpoints, FALSE).
#' @return
#'   A logical vector, the same length as \code{x} indicating if values fall
#'   within the specified range.
#' @export
#' @examples
#' inrange(4:6, c(4, 5.5))
#' inrange(4:6, c(4, 5.5), FALSE)

inrange <- function(x, r, open=TRUE) {
  if (open) {
    sel <- !is.na(x) & (x > r[1] & x < r[2])
  } else {
    sel <- !is.na(x) & x >= r[1] & x <= r[2]
  }
  sel
}
