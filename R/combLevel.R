#' Combine Levels
#'
#' Generates all possible combinations of a given number of levels.
#' @param n
#'   An integer scalar, the number of levels.
#' @return
#'   A matrix with \code{n} columns with one row for each possible combination
#'   of the \code{n} levels.
#' @export
#' @references
#'   Based on a method posted to the Adamistics blog 1 March 2016
#'   \href{http://adamistics.blogspot.com/2016/03/generating-combinations-of-levels.html}{[link]}.
#' @examples
#' combLevel(3)
#' combLevel(4)

combLevel <- function(n) {
  B <- matrix(1, nrow=1)
  for(i in 2:n) {
    maxB <- apply(B, 1, max) + 1
    B <- B[rep(1:nrow(B), maxB), ]
    B <- cbind(B, unlist(lapply(maxB, seq, 1, -1)))
  }
  dimnames(B) <- list(NULL, NULL)
  B
}
