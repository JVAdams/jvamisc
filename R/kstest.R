#' Kolmogorov-Smirnov Test
#'
#' Conduct a two-sample Kolmogorov-Smirnov test and output detailed
#' information on the results, including a plot of the distributions.
#' @param x
#'   A numeric vector representing the values from the first distribution.
#' @param y
#'   A numeric vector representing the values from the second distribution.
#' @param ids
#'   A character vector of length 2 assigning names to the two distributions.
#' @param leg.loc
#'   A character scalar denoting the location of the legend, specified as one
#'   of the following: "bottomright", "bottom", "bottomleft", "left", "topleft",
#'   "top", "topright", "right" (the default) and "center".
#' @param txt.loc
#'   A character scalar denoting the location of additional text, either "left"
#'   (the default) or "right".
#' @param cexall
#'   A numeric scalar giving the amount by which plotting text and symbols
#'   should be magnified, default 0.8.
#' @param cexmain
#'   A numeric scalar giving the amount by which the main text heading
#'   should be magnified, default 0.8.
#' @return
#'   A two panel plot of the smoothed probability and cumulative distributions
#'   is generated with a vertical line at the maximum distance and a summary
#'   of the Kolmogorov-Smirnov test annotated on the plot.
#' @seealso
#'   \code{\link{ks.test}}.
#' @export
#' @examples
#' myX <- rnorm(50, mean=3)
#' myY <- rnorm(20, mean=4)
#' kstest(x=myX, y=myY)

kstest <- function(x, y, ids=c("X", "Y"), leg.loc="right", txt.loc="left",
  cexall=0.8, cexmain=0.8) {

  # calculate maximum distance and where it occurs
  both <- sort(unique(c(x, y)))
  nx <- numeric(length(both))
  ny <- nx
  nx[match(sort(unique(x)), both)] <- table(x)
  ny[match(sort(unique(y)), both)] <- table(y)
  px <- cumsum(nx)/max(cumsum(nx))
  py <- cumsum(ny)/max(cumsum(ny))
  dist <- abs(px-py)
  md <- max(dist)

  # calculate smoothed densities for plots
  dx <- density(x, na.rm=T)
  dy <- density(y, na.rm=T)
  xr <- range(c(dx$x, dy$x))
  yr <- range(c(dx$y, dy$y))
  csxy <- cumsum(dx$y)/max(cumsum(dx$y))
  csyy <- cumsum(dy$y)/max(cumsum(dy$y))

  # define regions of the plots for text
  right <- quantile(c(dx$x, dy$x), 2/3)
  left <- min(c(dx$x, dy$x))
  p.upper <- quantile(c(dx$y, dy$y), 1)
  c.middle <- quantile(c(csxy, csyy), 1/2)
  txtx <- if(txt.loc=="right") right else left

  # carry out k-s test
  a <- ks.test(x, y)

  # plot the distributions
  par(mfrow=c(2, 1), mar=c(4, 4, 1, 1), oma=c(1, 1, 3, 1), cex=cexall)

  plot(dx$x, dx$y, type="l", xlab="Distribution", ylab="Probability Density",
    xlim=xr, ylim=yr)
  lines(dy$x, dy$y, lty=2)
  abline(v=both[dist==md], lty=3)
  legend(leg.loc, p.upper, ids, lty=c(1, 2), bty="n")

  plot(dx$x, csxy, type="l", xlim=xr, ylim=0:1,
    xlab="Distribution", ylab="Cumulative Probability Density")
  lines(dy$x, csyy, lty=2)
  abline(v=both[dist==md], lty=3)
  legend(leg.loc, c.middle, ids, lty=c(1, 2), bty="n")
  text(txtx, 0.7, paste("\nThe maximum distance, ", signif(md, 2),
    ",\nbetween the cumulative distribution\nfunctions occurred at ",
    signif(both, 4)[dist==md],
    ",\nwhere the cumulative proportions\nof x and y were ",
    signif(px, 2)[dist==md], " and ", signif(py, 2)[dist==md],
    ".\n\nKS statistic = ", signif(a$statistic, 4),
    "\np-value = ", signif(a$p.value, 4), sep=""), adj=0)

  mtext("Comparison of Distributions for\nTwo Sample Kolmogorov-Smirnov Test",
    outer=T, cex=cexmain)

  invisible()
}



