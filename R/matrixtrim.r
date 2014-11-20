#' Trim Matrix
#'
#' Trim the rows and columns of a logical matrix until every row and columns has a specified level of TRUE values.
#' @param m 		A logical matrix to be trimmed, should be all TRUE, FALSE (or 0, 1).
#' @param prop 		A numeric vector of length two, the minimum proportion of TRUE values in each row and column of the trimmed matrix, 
#' default c(1, 1).
#' @param rowsfirst	A logical scalar, indicating if rows (TRUE, the default) or columns (FALSE) should be trimmed from the matrix first.
#' @return 			A list of length three:
#' \itemize{
#'   \item \code{trim} = a logical matrix, the resulting trimmed matrix
#'   \item \code{dim} = a numeric vector of length 2, the dimensions of \code{trim}
#'   \item \code{n} = a numeric scalar, the total number of TRUE values in \code{trim}
#' }
#' @export
#' @examples 
#' m <- matrix(rep(c(1, 0, 1, 0), c(4, 1, 13, 2)), nrow=5)
#' matrixtrim(m)
#' matrixtrim(m, rowsfirst=FALSE)
#' matrixtrim(m, prop=c(0.7, 0.7))

matrixtrim <- function(m, prop=c(1, 1), rowsfirst=TRUE) {
	# trim the rows and columns of a 0/1 (or FALSE/TRUE) matrix
	# until every row and column has at least "prop" proportion of 1's (TRUE's)
	# method1 tends to delete columns first
	# method2 tends delete rows first
	newm <- m
	rmin <- 0
	cmin <- 0
	while(rmin < prop[1] | cmin < prop[2]) {
		rowmeans <- apply(newm, 1, mean)
		rmin <- min(rowmeans)
		rowofmin <- which(rowmeans==rmin)
		colmeans <- apply(newm, 2, mean)
		cmin <- min(colmeans)
		colofmin <- which(colmeans==cmin)
		if(rmin >= prop[1] & cmin >= prop[2]) break()
		if(rmin < cmin) {
			newm <- newm[-rowofmin, ]
			} else {
			if(rmin > cmin) {
				newm <- newm[, -colofmin]
				} else {
				if(rowsfirst) {
					newm <- newm[-rowofmin, ]
					} else {
					newm <- newm[, -colofmin]
					}
				}
			}
		}
	list(trim=newm, dim=dim(newm), n=sum(newm))
	}
