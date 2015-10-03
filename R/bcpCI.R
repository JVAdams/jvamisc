#' Bias-Corrected Percentile Confidence Interval
#'
#' Calculates the bias-corrected percentile confidence interval from
#' a bootstrap sample.
#' @param tboot
#'   A numeric vector of bootstrap estimates, typically \code{$t} from
#'   the output of the \code{\link{boot}} function.
#' @param orig
#'   A numeric scalar, the original estimate from the data,
#'   typically \code{$t0} from the output of the \code{\link{boot}} function.
#' @param alpha
#'   A numeric scalar, the desired significance level for
#'   100*(1-\code{alpha})\% confidence limits, default 0.05.
#' @return
#'   A named numeric vector of length 2, with the lower and
#'   upper confidence limits.
#' @export
#' @references
#'   Manly, Bryan F. J.  1997.
#'   Randomization, Bootstrap and Monte Carlo Methods in Biology.
#'   Chapman & Hall, London.
#' @examples
#' bcpCI(exp(rnorm(20)), 1)
#'
#' \dontrun{
#' # Bootstrap of the ratio of means using the city data
#' library(boot)
#' ratio <- function(d, w) sum(d$x * w)/sum(d$u * w)
#' results <- boot(city, ratio, R=999, stype="w")
#' results$t0
#' bcpCI(results$t, results$t0, 0.1)
#' }

bcpCI <- function(tboot, orig, alpha=0.05) {
  # Bias-corrected percentile 100*(1-alpha)% confidence limits
  tboot <- tboot[!is.na(tboot)]
  z0 <- qnorm(1-mean(tboot > orig))
  # proportion of times the bootstrap estimate exceeds the original estimate
  qn <- qnorm(1-alpha/2)
  lp <- pnorm(2 * z0 - qn)
  up <- pnorm(2 * z0 + qn)
  out <- quantile(tboot, c(lp, up))
  names(out) <- paste(format(100*c(alpha/2, 1-alpha/2)), "%", sep="")
  out
}
