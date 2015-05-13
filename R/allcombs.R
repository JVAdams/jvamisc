#' All Combinations
#'
#' All possible combinations of the given number of items.
#' @param num
#'   A scalar, the number of items.
#' @param from
#'   A scalar, the minimum number of items in each combination, default 0.
#' @param to
#'   A scalar, the maximum number of items in each combination,
#'   default \code{num}.
#' @return
#'   A matrix with rows corresponding to each possible combination and
#'   columns corresponding to item number.
#' @export
#' @references
#'   Based on a method posted by Petr Savicky on 20 Jul 2012 to R-help
#'   \href{https://stat.ethz.ch/pipermail/r-help/2012-July/319001.html}{[link]}.

#' @examples
#' allcombs(3)

allcombs <- function(num, from=0, to=num) {
	# create a matrix of all possible combinations of num items
	# restrict each row to have between "from" and "to" items
	# code based on posting to R-help by Petr Savicky, 20 Jul 2012
	m <- as.matrix(expand.grid(rep(list(0:1), times=num)))
	n.items <- rowSums(m)
	m[n.items >= from & n.items <= to, ]
}
