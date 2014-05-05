#' All combinations bbb
#'
#' All possible combinations of the given number of items.
#' @param num a scalar, the number of items
#' @param from a scalar, the minimum number of items in each combination, default 0
#' @param to a scalar, the maximum number of items in each combination, default \code{num}
#' @return a matrix with rows corresponding to each possible combination and columns corresponding to item number
#' @export
#' @examples all.combs(3)

all.combsb <- function(num, from=0, to=num) {
	num+from+to
	}
