#' Draw Circles on a Plot
#'
#' Draw circles on a plot, with control over circle size.
#' @param x 					Numeric vector of x coordinates.
#' @param y 					Numeric vector of y coordinates.
#' @param z 					Numeric vector of data used to generate circles.
#' @param data.range 			Numeric vector, length 2, minimum and maximum z data to plot, default \code{range(z, na.rm=TRUE)}.
#' @param circle.size.range 	Numeric vector, length 2, minimum and maximum circle radii in inches, default 0.1 to 1.
#' @param outx 					Numeric scalar of x coordinate beyond the figure margins, default NA (see details).
#' @param outy 					Numeric scalar of y coordinate beyond the figure margins, default NA (see details).
#' @param add 					Logical scalar specifying if circles are added to existing plot (TRUE), default FALSE (a new plot is created).
#' @param xlim 					Numeric vector, length 2, x-axis limits, unused if \code{add=TRUE}.
#' @param ylim 					Numeric vector, length 2, y-axis limits, unused if \code{add=TRUE}.
#' @param ... 					Additional parameters supplied to the \code{\link{symbols}} function.
#' @return 						A data frame with the name, class, dimension, and size of each member of the environment.
#' @details 
#'
#' The size of the circles plotted corresponds directly with the range of data.
#' For example, if there is a z of size \code{data.range[1]}, it will be plotted as a circle with radius \code{circle.size.range[1]},
#' and if there is a z of size \code{data.range[2]}, it will be plotted as a circle with radius \code{circle.size.range[2]}.
#'
#' The default of NA for \code{outx} and \code{outy} places unseen smallest and biggest circles at a location
#' where the x and y coordinates are 10 times the range observed plus the maximum observed.
#' In most instances this should be well beyond the figure margins.
#' @export
#' @examples 
#' circles(trees$Height, trees$Girth, trees$Volume, data.range=sqrt(c(0, max(trees$Volume))), 
#' 	circle.size.range=c(0, 0.3), xlab="Height (ft)", ylab="Diameter (in)", main="Tree Volume")

circles <- function(x, y, z, data.range=range(z, na.rm=TRUE), circle.size.range=c(0.1, 1), outx=NA, outy=NA, add=FALSE, xlim=NULL, ylim=NULL, ...) {
	# control the plotting of circles
	# fix the data range to be plotted (data.range)
	# and fix the corresponding range of circle sizes (circle.size.range)
	# rescale data range so it matches circle size range
	newz <- ((z - data.range[1])/diff(data.range)) * diff(circle.size.range) + circle.size.range[1]
	# add two data points that plot the min and max circle sizes outside the plot area
	if(is.na(outx)) outx <- max(x) + 10 * diff(range(x))
	if(is.na(outy)) outy <- max(y) + 10 * diff(range(y))
	addx <- c(x, outx, outx)
	addy <- c(y, outy, outy)
	addz <- c(newz, circle.size.range)
	# plot the circles
	if(add) {
		symbols(addx, addy, circles=addz, inches=circle.size.range[2], add=TRUE, ...)
		} else {
		if (is.null(xlim)) xlim <- extendrange(x, f=0.1)
		if (is.null(ylim)) ylim <- extendrange(y, f=0.1)
		symbols(addx, addy, xlim=xlim, ylim=ylim, circles=addz, inches=circle.size.range[2], add=FALSE, ...)
		}
	}
