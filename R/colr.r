#' Create a Range of Colors
#'
#' Create a range of colors between two specified colors.
#' @param x 			A numeric vector, the values to be assigned colors.
#' @param fromcolname	A character or numeric scalar, indicating the color to use for the lowest value in \code{x}.
#'	Either a color name (as listed by \code{\link{colors}()}, 
#'	a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see \code{\link{rgb}}), 
#'	or a positive integer i meaning \code{\link{palette}()[i]}.
#' @param fromcolname	A character or numeric scalar, indicating the color to use for the highest value in \code{x}.  
#'	See \code{fromcolname}.
#' @export
#' @import				plotrix
#' @seealso 			\code{\link{colors}}, \code{\link{palette}}.
#' @examples
#' x <- 1:10
#' plot(x, x, pch=16, col=colr(x, "blue", "yellow"), cex=4)

colr <- function(x, fromcolname, tocolname) {
	# recode x to a specified color range
	from <- col2rgb(fromcolname)
	to <- col2rgb(tocolname)
	red <- rescale(x, c(from[1], to[1]))
	green <- rescale(x, c(from[2], to[2]))
	blue <- rescale(x, c(from[3], to[3]))
	rgb(red, green, blue, maxColorValue=255)
	}
