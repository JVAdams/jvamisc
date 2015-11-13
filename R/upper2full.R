#' Convert Upper Triangular Matrix to Full Matrix
#'
#' Convert the upper triangular part of a matrix to a full symmetric matrix.
#' @param up
#'   A matrix in which the upper trianglular part is of interest.
#' @param diagval
#'   A vector of values to be used for the diagonal of the returned
#'   symmetric matrix.  May be the same length as each dimension of \code{up}
#'   or may be of length 1.  If Null, the default, the diagonal values will
#'   be taken from \code{up}.
#' @return
#'   A symmetric matrix the same dimensions as \code{up}, with diagonal values
#'   assigned according to \code{diagval}.
#' @export
#' @examples
#' m <- matrix(1:16, nrow=4, byrow=TRUE)
#' upper2full(m)
#' upper2full(m, 0)
#' upper2full(m, 1:4)
upper2full <- function(up, diagval=NULL) {
  up[lower.tri(up)] <- t(up)[lower.tri(t(up))]
  if (!is.null(diagval)) {
    diag(up) <- diagval
  }
  up
}
