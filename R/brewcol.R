#' Assign Colors to Collection of Values Using Color Brewer
#'
#' Assign a specified number of Color Brewer colors to a collection of values.
#' @param x
#'   A numeric vector of values to which colors will be assigned.
#' @param n
#'   An integer scalar indicating the number of unique colors to be assigned,
#'   default 9.  The range of possible values depends on the palette named,
#'   typically \code{n} should be from 3 to 9.
#' @param name
#'   A character scalar indicating the palette name to be passed to
#'   \code{\link{brewer.pal}}, default "YlGnBu".
#' @return
#'   A character vector of colors expressed as hexadecimal values with a "#"
#'   prefix.
#' @import RColorBrewer
#' @export
#' @examples
#' x <- 1:20
#' mycol <- brewcol(x, 4, "Dark2")
#' plot(x, x, col=mycol, pch=16, cex=3)

brewcol <- function(x, n=9,
  name=c("YlGnBu", "Oranges", "Greens", "Blues", "Greys", "Dark2")[1]) {
  y <- cut(x, breaks=n, labels=FALSE)
  brewer.pal(n, name)[y]
  }
