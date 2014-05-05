#' All combinations aaa
#'
#' @param num a scalar
#' @param from a scalar
#' @param to a scalar

all.combsa <- function(num, from=0, to=num) {
	# create a matrix of all possible combinations of num items
	# restrict each row to have between "from" and "to" items
	# code based on posting to R-help by Petr Savicky, 20 Jul 2012
	m <- as.matrix(expand.grid(rep(list(0:1), times=num))) 
	n.items <- rowSums(m) 
	m[n.items >= from & n.items <= to, ] 
	}
