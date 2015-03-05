#' Prettify the Numeric Columns of a Table
#'
#' Prettify the numeric columns of a table for printing.
#' @param m
#'   Two dimensional data frame, matrix, or table to be prettified.
#' @param sigdig
#'   Integer scalar, number of significant digits to be used, default 3.
#' @return
#'   A data frame, matrix, or table, the same dimensions as \code{m} with
#'   the numeric columns rounded to \code{sigdig} significant digits.
#' @seealso
#'   \code{\link{signif}}.
#' @export
#' @examples
#' head(mtcars)
#' prettytable(head(mtcars), 2)

prettytable <- function(m, sigdig=3) {
	mclass <- if (class(m)=="matrix") {
    apply(m, 2, class)
  } else {
    sapply(m, class)
  }
	nc <- mclass %in% c("numeric", "integer")
	if (sum(nc) > 0 ) {
		for(i in (1:dim(m)[2])[nc]) {
			m[, i] <- signif(m[, i], sigdig)
		}
	}
	m
}
