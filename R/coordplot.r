#' Plot Coordinates
#'
#' Quick plot of coordinates on equally scaled coordinate plane, labelled with row names. 
#' @param pts 	A matrix or data frame with the first two columns of numeric x and y coordinates to be plotted.
#' @seealso		\code{\link[jvamisc]{coordturn}}, \code{\link[jvamisc]{coordmove}}, \code{\link[jvamisc]{coordflip}}.
#' @import 		MASS
#' @export
#' @examples 
#' test <- matrix(c(0, 4, 1, 0, 2, 3), ncol=2, dimnames=list(LETTERS[1:3], NULL))
#' coordplot(test)

coordplot <- function(pts) {
	dev.new(rescale="fit")
	par(mar=0.5 + c(2, 2, 1, 1))
	eqscplot(pts[, 1], pts[, 2], type="n", las=1, xlab="", ylab="")
	labs <- if(is.null(dimnames(pts)[[1]])) 1:dim(pts)[1] else dimnames(pts)[[1]]
	text(pts[, 1], pts[, 2], labs)
	}
