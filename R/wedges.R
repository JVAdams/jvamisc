#' Add Wedge-Shaped Line Segments to a Plot
#'
#' Draw wedge-shaped line segments between pairs of points.
#' @param x0
#'   Numeric vector of x coordinates \strong{from} which to draw
#'   the wide base of the wedges.
#' @param y0
#'   Numeric vector of y coordinates \strong{from} which to draw
#'   the wide base of the wedges.  Must be the same length as \code{x0}.
#' @param x1
#'   Numeric vector of x coordinates \strong{to} which to draw
#'   the narrow point of the wedges.  Must be the same length as \code{x0}.
#' @param y1
#'   Numeric vector of y coordinates \strong{to} which to draw
#'   the narrow point of the wedges.  Must be the same length as \code{x0}.
#' @param ybase
#'   Numeric vector giving the width of the wedge bases in y-scale units,
#'   default 3% of the y-axis range.  Must be of length 1 or the same
#'   length as \code{x0}.
#' @param pborder
#'   Vector of colors to draw the borders of the wedges.
#'   The default, NA, omits borders.
#'   Must be of length 1 or the same length as \code{x0}.
#'   See \code{\link{polygon}}.
#' @param pcol
#'   Vector of colors to fill the wedges, default "darkgray".
#'   Must be of length 1 or the same length as \code{x0}.
#'   See \code{\link{polygon}}.
#' @param ...
#'   Additional arguments to \code{\link{polygon}}.
#' @export
#' @seealso
#'   \code{\link{segments}}, \code{\link{arrows}}, \code{\link{polygon}}.
#' @examples
#' df <- data.frame(x=runif(5, 0, 1), y=runif(5, 0, 10))
#'
#' # draw a single wedge
#' plot(df)
#' with(df, wedges(x[1], y[1], x[2], y[2], ybase=1))
#'
#' # draw several wedges from point to point
#' s <- seq(length(df$x)-1)
#' plot(df)
#' with(df, lapply(s, function(i)
#'  wedges(x[i], y[i], x[i+1], y[i+1], pborder=(5:2)[i], pcol=(2:5)[i])
#' ))
#'
wedges <- function(x0, y0, x1, y1, ybase=0.03*diff(par("usr")[3:4]),
  pborder=NA, pcol="darkgray", ...) {
  # plot dimensions
  usr <- par("usr")
  pin <- par("pin")
  # scales in units per inch
  yupi <- (usr[4] - usr[3]) / pin[2]
  xupi <- (usr[2] - usr[1]) / pin[1]
  # x2y conversion factor
  x2y <- yupi/xupi
  # convert x to y scale
  newx0 <- x0*x2y
  newx1 <- x1*x2y
  # distance between points
  L <- sqrt((newx0 - newx1)^2 + (y0 - y1)^2)
  # the sign changes in step 1 and step 2 below are to ensure that
  # the signs of dx and dy are the same for paired points oriented NE-SW
  # and different for those oriented NW-SE
  # step 1
  L[x0>x1] <- -L[x0>x1]
  # angle between points
  A <- asin((y1 - y0)/L)
  # delta x and y for base wedge
  dx <- sin(A)*ybase/2
  # step 2
  dy <- -cos(A)*ybase/2
  # wedge base coordinates
  newwbx1 <- newx0+dx
  newwbx2 <- newx0-dx
  wby1 <- y0+dy
  wby2 <- y0-dy
  # convert wedge base x coordinates back to x scale
  wbx1 <- newwbx1/x2y
  wbx2 <- newwbx2/x2y
  # draw the wedge
  if(length(x0)>1) {
    px <- as.vector(rbind(wbx1, wbx2, x1, div=NA))
    py <- as.vector(rbind(wby1, wby2, y1, div=NA))
  } else {
    px <- c(wbx1, wbx2, x1)
    py <- c(wby1, wby2, y1)
  }
  polygon(px, py, border=pborder, col=pcol, ...)
}
