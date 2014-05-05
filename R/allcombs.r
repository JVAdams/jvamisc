#' All combinations
#'
#' All possible combinations of the given number of items.
#' @param num a scalar, the number of items
#' @param from a scalar, the minimum number of items in each combination, default 0
#' @param to a scalar, the maximum number of items in each combination, default \code{num}
#' @return a matrix with rows corresponding to each possible combination and columns corresponding to item number
#' @export
#' @examples allcombs(3)

allcombs <- function(num, from=0, to=num) {
	# create a matrix of all possible combinations of num items
	# restrict each row to have between "from" and "to" items
	# code based on posting to R-help by Petr Savicky, 20 Jul 2012
	m <- as.matrix(expand.grid(rep(list(0:1), times=num))) 
	n.items <- rowSums(m) 
	m[n.items >= from & n.items <= to, ] 
	}
