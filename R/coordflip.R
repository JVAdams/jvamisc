#' Flip Coordinates
#'
#' Flip (or reflect) coordinates across any given line.
#' @param pts
#'   A numeric matrix with two columns of x and y coordinates to be rotated.
#' @param seg
#'   A numeric matrix of dimension 2 x 2 giving two points which define
#'   the line over which the coordinates will be flipped.
#' @return
#'   A numeric matrix with same dimension as \code{pts} with
#'   the flipped x and y coordinates.
#' @export
#' @seealso
#'   \code{\link[jvamisc]{coordplot}}, \code{\link[jvamisc]{coordmove}},
#'   \code{\link[jvamisc]{coordturn}}, \code{\link[jvamisc]{coordtri}}.
#' @references
#'   Based on a method posted by Il-Bhima on 22 July 2010 on stackoverflow
#'   \href{http://stackoverflow.com/questions/3306838/algorithm-for-reflecting-a-point-across-a-line}{[link]}.
#' @examples
#' # starting coordinates
#' test <- matrix(c(0, 4, 1, 0, 2, 3), ncol=2,
#'  dimnames=list(LETTERS[1:3], NULL))
#' coordplot(test)
#' # flip the coordinates across the line defined by the first two points
#' ftest <- coordflip(test, test[1:2, ])
#' coordplot(ftest)

coordflip <- function(pts, seg) {
	# equation of line through seg
	# slope=delta y / delta x
  m <- (seg[2, 2] - seg[1, 2]) / (seg[2, 1] - seg[1, 1])
	# intercept=y - mx
	b <- seg[1, 2] - m * seg[1, 1]
	# "flip" coordinates around line
	d <- (pts[, 1] + (pts[, 2] - b)*m) / (1 + m^2)
	newx <- 2*d - pts[, 1]
	newy <- 2*d*m - pts[, 2] + 2*b
	m <- cbind(newx, newy)
	dimnames(m) <- dimnames(pts)
	m
}
