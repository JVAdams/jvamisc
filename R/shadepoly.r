#' Add Shaded Polygon to Plot
#'
#' Add a spline-smoothed, shaded polygon to a plot, typically to show an interval or range of values (in the y-direction) for a time series of x values.
#' @param x 		A numeric vector, the metric to plot in the x direction (e.g., time).
#' @param ymd 		A numeric vector, the metric to plot in the y direction (e.g., a mean or median).
#' @param ylo 		A numeric vector, the lower interval or range in the y direction (e.g., a lower confidence interval or quartile).
#' @param yhi 		A numeric vector, the upper interval or range in the y direction (e.g., an upper confidence interval or quartile).
#' @param subsel	A logical vector, indicating subset of the data to plot.
#' @param kol		A character scalar, the hex color to use for plotting both the shaded polygon and (if requested) the line, default "#000000" (black).
#' @param opq		A numeric vector of length 2, opacity for the polygon and the line, default c(20, 50).
#' @param addline	A logical scalar, indicating if the x vs ymd line should be added to the plot (on top of the shaded polygon), default TRUE.
#' @details			Missing values are removed prior to plotting, such that there will be no breaks in the shaded polygon nor the line (if requested).
#' The lower and upper intervals of the polygon are spline-smoothed prior to plotting, as is the line (if requested).
#' @export
#' @seealso 		\code{\link{polygon}}, \code{\link{lines}}.
#' @examples
#' x <- 1:10
#' y <- sample(10)
#' noise <- abs(rnorm(10))
#' plot(x, y, ylim=range(y-noise, y+noise), type="n")
#' shadepoly(x, y, y-noise, y+noise)

shadepoly <- function(x, ymd, ylo, yhi, subsel=NULL, kol="#000000", opq=c(20, 50), addline=TRUE) {
	if(is.null(subsel)) {
		subsel2 <- !is.na(ymd)
		} else {
		subsel2 <- subsel & !is.na(ymd)
		}
	a <- spline(x[subsel2], ylo[subsel2], n=100)
	b <- spline(x[subsel2], yhi[subsel2], n=100)
	polygon(c(a$x, rev(b$x)), c(a$y, rev(b$y)), col=paste0(kol, opq[1]), border=NA)
	if(addline) lines(spline(x[subsel2], ymd[subsel2], n=100), col=paste0(kol, opq[2]), lwd=2)
	invisible()
	}
