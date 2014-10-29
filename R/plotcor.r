#' Plot a Matrix of Correlations
#'
#' Plot a matrix of correlations using white ellipses (representing the measured correlation) overlaid on colored circles.  
#' @param r 		A matrix of correlations (need not be symmetrical) with values ranging from -1 to 1.
#' @param addtext 	A logical scalar indicating whether the value of each correlation should be written over the ellipses, default TRUE.
#' @param atcex 	A numeric scalar giving the magnification of the added ellipse text relative to that set in \code{\link{par}},
#'	default NULL results in atcex = 8/maximum dimension of \code{r}.
#' @param incdiag 	A logical scalar indicating whether to plot circles for the diagonal values of the matrix (if symmetric), default FALSE.
#' @param rorder 	A logical scalar indicating whether the columns and rows of the matrix should be reordered using seriation, default TRUE.
#' @param ... 		Additional parameters to \code{\link{par}}.
#' @details			
#' Each ellipse is sized so that the proportion of the colored circle visible beyond the ellipse is equal to the squared correlation.
#' Each ellipse is oriented northeast for positive correlation and northwest for negative correlation.
#' The color of each circle ranges from cyan (for correlation = -1) to magenta (for correlation = 1) through white
#' (for correlation = 0).  Similarly, the transparency of each correlation value (if addtext=TRUE) ranges from 1 (for correlation = 0) to
#' 0 (for absolute correlation = 1).
#' @return 			A list of with two vector of integers (the same length as each dimension of \code{r}) representing the linear order suggested by seriation.
#' @export
#' @seealso 		\code{\link[ellipse]{plotcorr}}, on which the idea for the function was based, \code{\link{seriation}},
#'	and \code{\link[plotrix]{draw.ellipse}}.
#' @import 			plotrix seriation MASS
#' @examples
#' # example using a symmetric matrix
#' sr <- cor(swiss)
#' sord <- plotcor(sr)
#' sr[sord[[1]], sord[[2]]]
#' # example using an asymmetric matrix
#' lr <- cor(longley)[1:3, 4:7]
#' lord <- plotcor(lr)
#' lr[lord[[1]], lord[[2]]]

plotcor <- function(r, addtext=TRUE, atcex=NULL, incdiag=FALSE, rorder=TRUE, ...) {

	# round to the nearest hundredth
	rr <- round(r, 2)

	dimr <- dim(r)
	sym <- isSymmetric(r)

	# get rid of diagonal numbers
	if(!incdiag & sym) diag(rr) <- NA

	rrf <- format(rr)
	rrf[grep("NA", rrf)] <- ""
	rra <- abs(rr)
	nx <- dimr[2]
	ny <- dimr[1]
	if(is.null(atcex)) atcex <- 8/max(nx, ny)
	namzx <- dimnames(rr)[[2]]
	namzy <- dimnames(rr)[[1]]

	# order rows/columns
	ordx <- 1:nx
	ordy <- 1:ny
	if(rorder) {
		# the seriate() function prints out % explained variance for method="PCA"
		# I used capture.output to avoid having this print to the screen
		dummy <- capture.output(ser <- seriate((1-r)/2, method="PCA"))
		ordy <- rev(get_order(ser, 1))
		ordx <- rev(get_order(ser, 2))
		}
	if(sym) ordx <- rev(ordy)

	# categorize correlations from -1 to 1 by 0.01
	brks <- seq(-1, 1, 0.01)
	rcat <- apply(rr, 2, cut, breaks=brks, labels=FALSE)

	# assign colors on the cyan-magenta scale
	colz <- apply(rcat, 2, function(x) cm.colors(length(brks))[x])
	par(xaxs="i", yaxs="i", mar=c(0.1, 7, 7, 0.1), ...)
	eqscplot(1, 1, type="n", xlim=c(0.5, nx+0.5), ylim=c(0.5, ny+0.5), xlab="", ylab="", axes=FALSE)
	for(i in 1:nx) {
	for(j in 1:ny) {
		io <- ordx[i]
		jo <- ordy[j]
		draw.ellipse(i, j, a=0.5, b=0.5, col=colz[jo, io], border=NA)
		draw.ellipse(i, j, a=0.5, b=(1-rr[jo, io]^2)/2, angle=45*c(-1, 1)[1+(rr[jo, io]>0)], col="white", border=NA)
		if(addtext & !is.na(rra[jo, io])) text(i, j, rrf[jo, io], cex=atcex, col=rgb(0, 0, 0, alpha=rra[jo, io]))
		}}
	axis(3, at=1:nx, labels=namzx[ordx], las=2, tick=FALSE)
	axis(2, at=1:ny, labels=namzy[ordy], las=2, tick=FALSE)

	list(rev(ordy), ordx)
	}
