#' Determine Third Coordinate of Triangle
#'
#' Determine the third coordinate of a triangle, given the other two coordinates and the distances to the third.
#' @param cleft 	A numeric vector of length 2, the x and y coordinates of the lower left point of the triangle.
#' @param cright 	A numeric vector of length 2, the x and y coordinates of the lower right point of the triangle.
#' @param lleft 	A numeric scalar, the length of the left side of the triangle, from \code{cleft} to the top.
#' @param lright 	A numeric scalar, the length of the right side of the triangle, from \code{cright} to the top.
#' @return 			A numeric vector of length 2, the x and y coordinates of the top point of the triangle.
#' @seealso			\code{\link[jvamisc]{coordplot}}, \code{\link[jvamisc]{coordturn}}, \code{\link[jvamisc]{coordmove}}, \code{\link[jvamisc]{coordflip}}.
#' @export
#' @examples 
#' # define coordinates of base of triangle
#' AB <- rbind(A=c(-2, 4), B=c(2, -4))
#' # determine coordinates of top of triangle, given base coordinates and side lengths
#' C <- coordtri(cleft=AB[1, ], cright=AB[2, ], lleft=12, lright=10)
#' # plot results
#' coordplot(rbind(C, AB))

coordtri <- function(cleft, cright, lleft, lright) {
	# find third coordinate of triangle
	# cleft, cright = coordinate of left, right base of triangle
	# lleft, lright = length of left, right segment of triangle
	cbot <- rbind(cleft, cright)
	lbot <- dist(cbot)
	# move so that cleft is at origin (0, 0)
	cbot2 <- coordmove(cbot, cleft, c(0, 0))
	# rotate so that cright2 is at (0, dist(cleft2, cright2))
	if(abs(cbot[2, 2]) < 1e-12) {
		if(cbot[2, 1] < -1e-12) {
			ang <- pi
			} else {
				ang <- 0
				}
		} else {
		ang <- atan(cbot2[2, 2]/cbot2[2, 1])
		}
	cbot3 <- coordturn(cbot2, c(0, 0), ang)
	# calculate location of triangle top
	ctop3 <- matrix(numeric(2), ncol=2)
	ctop3[, 1] <- (lbot^2 + lleft^2 - lright^2) / (2*lbot)
	ctop3[, 2] <- sqrt(lleft^2 - ctop3[, 1]^2)
	# backrotate
	ctop2 <- coordturn(ctop3, c(0, 0), -ang)
	# backmove
	ctop <- coordmove(ctop2, c(0, 0), cleft)
	as.numeric(ctop)
	}
