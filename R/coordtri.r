#' Determine Third Coordinate of Triangle
#'
#' Determine the third coordinate of a triangle, given the other two coordinates and the distances to the third.
#' @param cleft 	A numeric vector of length 2, the x and y coordinates of the lower left point of the triangle (see details).
#' @param cright 	A numeric vector of length 2, the x and y coordinates of the lower right point of the triangle (see details).
#' @param lleft 	A numeric scalar, the length of the left side of the triangle, from \code{cleft} to the top (see details).
#' @param lright 	A numeric scalar, the length of the right side of the triangle, from \code{cright} to the top (see details).
#' @return 			A numeric vector of length 2, the x and y coordinates of the top point of the triangle.
#' @seealso			\code{\link[jvamisc]{coordplot}}, \code{\link[jvamisc]{coordturn}}, \code{\link[jvamisc]{coordmove}}, \code{\link[jvamisc]{coordflip}}.
#' @export
#' @details
#'
#' The directions "left" and "right" refer to the triangle not necessarily in its current state, 
#' but rather \strong{after} it has been rotated such that the bottom of the triangle is level (e.g., on the y=0 line), 
#' and the third coordinate that the function returns is "above" the bottom (e.g., y>0).  
#'
#' A warning message is given if a triangle can't be built from the inputs provided, and the returned value
#' is \code{c(NA, NA)}.
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
	if(any(is.na(cleft)) | any(is.na(cright)) | is.na(lleft) | is.na(lright)) {
		warning("Can't build a triangle with missing values in the inputs.")
		res <- c(NA, NA)
		} else {
		cbot <- rbind(cleft, cright)
		lbot <- dist(cbot)
		if(lbot > (lleft + lright) | lleft > (lbot + lright) | lright > (lbot + lleft)) {
			lord <- paste0(format(signif(as.numeric(sort(c(lbot, lleft, lright))), 3)), collapse=", ")
			warning(paste0("Can't build a triangle with side lengths of ", lord, "."))
			res <- c(NA, NA)
			} else {
			# move so that cleft is at origin (0, 0)
			cbot2 <- coordmove(cbot, cleft, c(0, 0))
			# rotate so that cright2 is at (0, dist(cleft2, cright2))
			if(abs(cbot2[2, 2]) < 1e-12) {
				if(cbot2[2, 1] < -1e-12) {
					ang <- pi
					} else {
						ang <- 0
						}
				} else {
				ang <- atan(cbot2[2, 2]/cbot2[2, 1])
				}
			cbot3 <- coordturn(cbot2, c(0, 0), ang)
			# if xcoord of cright3 is negative, change its sign and adjust ang by pi
			if(cbot3[2, 1] < -1e-12) {
				cbot3 <- abs(cbot3)
				ang <- ang - pi
				}
			# calculate location of triangle top
			ctop3 <- matrix(numeric(2), ncol=2)
			ctop3[, 1] <- (lbot^2 + lleft^2 - lright^2) / (2*lbot)
			ctop3[, 2] <- sqrt(lleft^2 - ctop3[, 1]^2)
			# backrotate
			ctop2 <- coordturn(ctop3, c(0, 0), -ang)
			# backmove
			ctop <- coordmove(ctop2, c(0, 0), cleft)
			res <- as.numeric(ctop)
			}
		}
	res
	}
