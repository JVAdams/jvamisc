#' Plot a Correlation Matrix
#'
#' Plot a correlation matrix using white ellipses (representing the measured correlation) overlaid on colored circles.  
#' @param r 		A symmetric correlation matrix with values ranging from -1 to 1.
#' @param addtext 	A logical scalar indicating whether the value of each correlation should be written over the ellipses, default TRUE.
#' @param incdiag 	A logical scalar indicating whether to plot circles for the diagonal values of the matrix, default FALSE.
#' @param rorder 	A logical scalar indicating whether the columns and rows of the matrix should be reordered using seriation, default TRUE.
#' @param ... 		Additional parameters to \code{\link{par}}.
#' @details			
#' Each ellipse is sized so that the proportion of the colored circle visible beyond the ellipse is equal to the squared correlation.
#' Each ellipse is oriented northeast for positive correlation and northwest for negative correlation.
#' The color of each circle ranges from cyan (for correlation = -1) to magenta (for correlation = 1) through white
#' (for correlation = 0).  Similarly, the transparency of each correlation value (if addtext=TRUE) ranges from 1 (for correlation = 0) to
#' 0 (for absolute correlation = 1).
#' @return 			A vector of integers (the same length as each dimensions of \code{r}) representing the linear order suggested by seriation.
#' @export
#' @seealso 		\code{\link[ellipse]{plotcorr}}, on which the idea for the function was based, and \code{\link{seriation}}.
#' @import 			plotrix seriation
#' @examples
#' plotcor(cor(swiss))
#' plotcor(cor(longley))

plotcor <- function(r, addtext=TRUE, incdiag=FALSE, rorder=TRUE, ...) {

	# round to the nearest hundredth
	rr <- round(r, 2)

	# get rid of diagonal numbers
	if(!incdiag) diag(rr) <- NA

	rrf <- format(rr)
	rrf[grep("NA", rrf)] <- ""
	rra <- abs(rr)
	n <- dim(rr)[1]
	namz <- dimnames(rr)[[1]]

	# order rows/columns
	ord1 <- 1:n
	if(rorder) {
		ser <- seriate((1-r)/2)
		ord1 <- get_order(ser, 1)
		}

	# categorize correlations from -1 to 1 by 0.01
	brks <- seq(-1, 1, 0.01)
	rcat <- apply(rr, 2, cut, breaks=brks, labels=FALSE)

	# assign colors on the cyan-magenta scale
	colz <- apply(rcat, 2, function(x) cm.colors(length(brks))[x])
	escale <- rep(1/2.5/2, 2)
	par(xaxs="i", yaxs="i", pty="s", mar=c(7, 7, 0.1, 0.1), ...)
	plot(1:n, 1:n, type="n", xlim=c(0.5, n+0.5), ylim=c(0.5, n+0.5), xlab="", ylab="", axes=FALSE)
	for(i in 1:n) {
	for(j in 1:n) {
		io <- ord1[i]
		jo <- ord1[j]
		draw.ellipse(i, j, a=0.5, b=0.5, col=colz[io, jo], border=NA)
		draw.ellipse(i, j, a=0.5, b=(1-rr[io, jo]^2)/2, angle=45*c(-1, 1)[1+(rr[io, jo]>0)], col="white", border=NA)
		if(addtext & !is.na(rra[io, jo])) text(i, j, rrf[io, jo], cex=8/n, col=rgb(0, 0, 0, alpha=rra[io, jo]))
		}}
	axis(1, at=1:n, labels=namz[ord1], las=2, tick=FALSE)
	axis(2, at=1:n, labels=namz[ord1], las=2, tick=FALSE)

	ord1
	}
