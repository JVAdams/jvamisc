#' Mode of Continuous Variable
#'
#' Estimates the mode of a continuous variable.
#' @param x
#'   A numeric vector.
#' @param plot
#'   A logical scalar indicating whether to plot the results, default FALSE.
#' @param ...
#'   Additional arguments to the \code{\link{density}} function.
#' @return
#'   The estimated mode of \code{x}.
#' @export
#' @references
#'   Based on a method posted by Peter Dalgaard on 29 Aug 2008 on
#'   \href{https://stat.ethz.ch/pipermail/r-help/2008-August/172319.html}{r-help}.
#' @examples
#' x <- rnorm(100)
#' modecontin(x, TRUE)

modecontin <- function(x, plot=FALSE, ...) {
  dd <- density(x, ...)
  mode <- dd$x[which.max(dd$y)]
  if (plot) {
    plot(dd)
    rug(x)
    abline(v=mode)
  }
  return(mode)
}
