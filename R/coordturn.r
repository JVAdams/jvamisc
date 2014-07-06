#' Rotate Coordinates
#'
#' Rotate coordinates clockwise around a pivot point.  
#' @param pts 	A numeric matrix with two columns of x and y coordinates to be rotated.
#' @param pvt 	A numeric vector of length 2, the x and y coordinates of the pivot point around which \code{pts} will be rotated.
#' @param rot	A numeric scalar indicating the amount of clockwise rotation, in radians.
#' @return 		A numeric matrix with same dimension as \code{pts} with the rotated x and y coordinates.
#' @export
#' @seealso		\code{\link[jvamisc]{coordplot}}, \code{\link[jvamisc]{coordmove}}, \code{\link[jvamisc]{coordflip}}, \code{\link[jvamisc]{coordtri}}.
#' @references Modification of code posted by \strong{Sage} on 3 March 2011 on the website 
#' \href{http://benn.org/2007/01/06/rotating-coordinates-around-a-centre/}{benn.org}.
#' @examples 
#' # starting coordinates
#' test <- matrix(c(0, 4, 1, 0, 2, 3), ncol=2, dimnames=list(LETTERS[1:3], NULL))
#' coordplot(test)
#' # rotate the coordinates clockwise 45 degrees around the first point (the origin)
#' rottest <- coordturn(test, test[1, ], rot=pi/4)
#' coordplot(rottest)
#' # rotate the coordinates counterclockwise 45 degrees around the first point (the origin)
#' rottest <- coordturn(test, test[1, ], rot=-pi/4)
#' coordplot(rottest)

coordturn <- function(pts, pvt, rot) {
	r1 <- -rot
	xRot <- pvt[1] + cos(r1) * (pts[, 1] - pvt[1]) - sin(r1) * (pts[, 2] - pvt[2])
 	yRot <- pvt[2] + sin(r1) * (pts[, 1] - pvt[1]) + cos(r1) * (pts[, 2] - pvt[2])
	m <- cbind(xRot, yRot)
	dimnames(m) <- dimnames(pts)
	m
	}
