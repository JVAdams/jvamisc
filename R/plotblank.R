#' Create a Blank Plot
#'
#' Create a blank plot (no symbols) on which to add other plotting features.
#'
#' @param x
#'   A numeric vector, the x coordinates of points in the plot.
#' @param y
#'   A numeric vector, the y coordinates of points in the plot.
#' @param xlab
#'   A character scalar, title for the x axis, default "".
#' @param ylab
#'   A character scalar, title for the y axis, default "".
#' @param las
#'   A numeric scalar, style of axis labels, 0=always parallel to the axis,
#'   1=always horizontal (default), 2=always perpendicular to the axis,
#'   3=always vertical.
#' @param ...
#'   Additional arguments to the \code{\link{plot}} function.
#' @export
#' @seealso
#'   \code{\link{plot}}, \code{\link{title}}, \code{\link{par}}
#' @examples
#'
#' plotblank(xlim=c(1, 100))

plotblank <- function(x=0:1, y=0:1, xlab="", ylab="", las=1, ...) {
	plot(x, y, type="n", xlab=xlab, ylab=ylab, las=las, ...)
}
