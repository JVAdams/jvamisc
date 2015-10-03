#' Move Coordinates
#'
#' Move coordinates in the x and y direction in the plane.
#' @param pts
#'   A numeric matrix with two columns of x and y coordinates to be moved.
#' @param from
#'   A numeric vector of length 2, the starting x and y coordinates of a point.
#' @param to
#'   A numeric vector of length 2, the ending x and y coordinates of a point.
#' @return
#'   A numeric matrix with same dimension as \code{pts} with
#'   the moved x and y coordinates.
#' @seealso
#'   \code{\link[jvamisc]{coordplot}}, \code{\link[jvamisc]{coordturn}},
#'   \code{\link[jvamisc]{coordflip}}, \code{\link[jvamisc]{coordtri}}.
#' @export
#' @examples
#' test <- matrix(c(0, 4, 1, 0, 2, 3), ncol=2,
#'  dimnames=list(LETTERS[1:3], NULL))
#' coordplot(test)
#' # move the coordinates so that the second point is at the origin
#' mtest <- coordmove(test, test[2, ], c(0, 0))
#' coordplot(mtest)

coordmove <- function(pts, from, to) {
  adj <- to - from
  t(t(pts) + adj)
}
