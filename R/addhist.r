#' Add a Histogram
#'
#' Add a marginal histogram to a plot.
#' @param x 		A numeric vector, the first set of data to be binned into a histogram.
#' @param y 		A numeric vector, the second set of data to be binned into a histogram, default NULL.
#' @param type 		A character scalar, indicating to which plot axes histograms should be added; \code{"x"} adds a histogram along the x-axis, 
#' \code{"y"} adds a histogram along the y-axis, \code{"xy"} (the default) adds a histogram along both axes.
#' @param nclass 	An integer scalar, the target number of bins for the histogram, default 20.
#' @param newmar 	A numeric vector of length 2, indicating new margins to use, default c(0, 1).
#' @param adj.fac	A numeric scalar, adjustment factor for extent of y-axis, default 1.05 (to ensure tallest bars in histogram are below axis box).
#' @param xlab 		A character scalar, label for x-axis, default "Frequency".
#' @param ylab 		A character scalar, label for y-axis, default "".
#' @param fill 		A character or numeric scalar, color for filling in histogram bars, default "gray".
#' @export
#' @seealso 		\code{\link{hist}}.
#' @examples
#' # fake data
#' xx <- rnorm(30)
#' yy <- runif(30)
#' 
#' # type "x"
#' layout(matrix(2:1, ncol=1), heights=c(1/5, 4/5)) 
#' par(mar=c(4, 4, 0, 1), cex=1, las=1)
#' plot(xx, yy)
#' addhist(xx, type="x")
#' 
#' # type "y"
#' layout(matrix(1:2, ncol=2), widths=c(4/5, 1/5)) 
#' par(mar=c(4, 4, 1, 0), cex=1, las=1)
#' plot(xx, yy)
#' addhist(yy, type="y")
#' 
#' # type "xy"
#' layout(matrix(c(2, 1, 0, 3), ncol=2), heights=c(1/5, 4/5), widths=c(4/5, 1/5)) 
#' par(mar=c(4, 4, 0, 0), cex=1, las=1)
#' plot(xx, yy)
#' usr <- par("usr")
#' mar <- par("mar")
#' addhist(xx, yy, type="xy")

addhist <- function(x, y=NULL, type="xy", nclass=20, newmar=0:1, adj.fac=1.05, xlab="Frequency", ylab="", fill="gray") {
	oldmar <- par("mar")
	oldusr <- par("usr")
	xhist <- hist(x, nclass=nclass, plot=FALSE) 
	if(type=="xy" | type=="x") {
		par(mar=c(newmar[1], oldmar[2], newmar[2], oldmar[4]), xaxs="i", yaxs="i") 
		plot(xhist$mids, xhist$counts, type="n", xlim=oldusr[1:2], ylim=adj.fac*c(0, max(xhist$counts)), axes=FALSE, xlab=xlab, ylab=ylab)
		symbols(xhist$mids, xhist$counts/2, rectangles=cbind(diff(xhist$mids)[1], xhist$counts), inches=FALSE, bg=fill, add=TRUE)
		px <- pretty(xhist$counts)
		axis(2, at=px[-1])
		box()
		}
	if(type=="xy" | type=="y") {
		if(type=="xy") xhist <- hist(y, nclass=nclass, plot=FALSE) 
		par(mar=c(oldmar[1], newmar[1], oldmar[3], newmar[2]), xaxs="i", yaxs="i") 
		plot(xhist$counts/2, xhist$mids, type="n", xlim=adj.fac*c(0, max(xhist$counts)), ylim=oldusr[3:4], axes=FALSE, xlab=xlab, ylab=ylab)
		symbols(xhist$counts/2, xhist$mids, rectangles=cbind(xhist$counts, diff(xhist$mids)[1]), inches=FALSE, bg=fill, add=TRUE)
		px <- pretty(xhist$counts)
		axis(1, at=px[-1])
		box()
		}
	invisible()
	}
