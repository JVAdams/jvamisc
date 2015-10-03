#' Confidence Interval of Mean
#'
#' Calculate the confidence interval or limits of the mean using the
#' t distribution.
#' @param x
#'   Numeric vector of sample observations.
#' @param alpha
#'   Numeric scalar denoting significance level, default 0.05.
#' @param keepMean
#'   Logical scalar indicating if the mean should be returned with the
#'   confidence interval/limits, default TRUE.
#' @param limits
#'   Logical scalar indicating if the limits should be returned
#'   (otherwise a single interval is returned), default TRUE.
#' @param prefix
#'   Character scalar to be used in assigning names to the returned value,
#'   default "".
#' @param na.rm
#'   Logical scalar indicating if missing values should be removed before
#'   calculations, default FALSE.
#' @return
#'   A named vector of the mean (if \code{keepMean=TRUE}) and the
#'   (1 - \code{alpha})*100% confidence limits (if \code{limits=TRUE}) or
#'   interval (if \code{limits=FALSE}).
#'   Names are the \code{prefix} concatenated to "mean", "lo", and "hi".
#' @export
#' @examples
#' CI(1:10)

CI <- function(x, alpha=0.05, keepMean=TRUE, limits=TRUE, prefix="",
  na.rm=FALSE) {
  y <- if (na.rm) {
    x[!is.na(x)]
  } else {
    x
  }
  s <- sqrt(var(y))
  n <- length(y)
  ci <- if (n>1) {
    qt(1- alpha/2, n-1) * s / sqrt(n)
  } else {
    0
  }
  if (limits) {
    z <- c(mean(y), mean(y) + c(-1, 1) * ci)
    names(z) <- paste0(prefix, c("mean", "lo", "hi"))
    if (keepMean) {
      z 
    } else {
      z[2:3]
    }
  } else {
    ci
  }
}
