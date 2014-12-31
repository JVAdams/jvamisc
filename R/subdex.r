#' Subset Index
#'
#' Return the index of vectors, matrices or data frames which meet conditions.
#' @param x 		Vector, matrix, or data frame to be subsetted.
#' @param subset 	Logical expression indicating elements or rows to keep; missing values are taken as FALSE.
#' @return 			Logical vector of indices, the same length as \code{x} (if \code{x} is a vector) or the rows of \code{x} (if \code{x} is a matrix or data frame).
#' @export
#' @seealso 		\code{\link{subset}}, from which the function was derived.
#' @examples 
#' df <- data.frame(x=c(1, 2, NA, 4, 20), y=c(5, NA, 6, 30, 4))
#' subdex(df, x > 8 | y > 8)
#' df[subdex(df, x > 8 | y > 8), ]
#' # compare to subset
#' subset(df, x > 8 | y > 8)
#' 
#' m <- as.matrix(df)
#' subdex(m, x > 8 | y > 8)
#' m[subdex(m, x > 8 | y > 8), ]
#' # compare to subset
#' subset(m, m[, "x"] > 8 | m[, "y"] > 8)
#' 
#' z <- c(1, 2, NA, 4, 20)
#' subdex(z, z > 8)
#' z[subdex(z, z > 8)]
#' # compare to subset
#' subset(z, z > 8)

subdex <- function(x, subset) {
	# index of selected subset
	# modified version of subset.data.frame
	r <- if(missing(subset)) {
		rep_len(TRUE, nrow(x))
		} else {
		if(is.null(dim(x))) {
			subset & !is.na(subset)
			} else {
			if(class(x)=="matrix") x <- as.data.frame(x)
			e <- substitute(subset)
			r <- eval(e, x, parent.frame())
			if (!is.logical(r)) stop("'subset' must be logical")
			r & !is.na(r)
			}
		}
	r
	}
