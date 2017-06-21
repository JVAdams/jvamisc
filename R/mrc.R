#' Estimates from Mark-Recapture Studies
#'
#' Estimates the recovery rate and abundance of a closed population from
#' a mark-recapture study with a single capture and marking event and
#' a single recapture event.
#' @param M
#'   A numeric scalar, the number of individuals marked and released.
#' @param R
#'   A numeric scalar, the number of marked individuals recaptured.
#' @param C
#'   A numeric scalar, the number of individuals checked for marks.
#' @param alpha
#'   A scalar, the desired confidence level, default \code{0.05}.
#' @param ...
#'   Not in use, added for use with \code{plyr::mdply()} function.
#' @return
#'   A named vector with the estimated recovery rate \code{U} with
#'   lower and upper 100*(1-\code{alpha})\% confidence limits from
#'   \code{\link{binomCI}}, and the population estimate \code{N} and associated
#'   variance \code{V}
#'   (the bias-corrected Petersen estimator from Chapman 1954 Table 3)
#'   with the coefficient of variation \code{CV} and the
#'   lower and upper 100*(1-\code{alpha})\% confidence limits from
#'   the normal approximation of the Poisson for \code{R} (Ricker 1975).
#' @export
#' @seealso
#'   \code{\link{binomCI}}
#' @examples
#' mrc(100, 10, 1000)
#' df <- data.frame(Year=2001:2003, M=(1:3)*100, C=(2:4)*1000, R=(3:5)*10)
#' plyr::mdply(df, mrc)
#' @references
#' D. G. Chapman.  1954.
#'   The estimation of biological populations.
#'   Annals of Mathematical Statistics 25:1-15.
#'   \href{http://projecteuclid.org/euclid.aoms/1177728844}{[link]}.
#'
#' W. E. Ricker.  1975.
#'   Computation and interpretation of biological statistics of
#'     fish populations.
#'   Fisheries Research Board of Canada Bulletin. 191.

mrc <- function(M, R, C, alpha=0.05, ...) {
  if(C < M | C < R | M < R) warning("This inequality was not true: C >= M >= R")
  U <- binomCI(R, M-R, alpha=alpha)[1:3]
  N <- (M+1) * (C+1) / (R+1) - 1
  V = (M+1)^2*(C+1)*(C-R)/((R+1)^2*(R+2))
  CV = 100*sqrt(V)/N
  CI <- (M+1) * (C+1) / (R+1+(c(1, -1)*qnorm(1-alpha/2)*sqrt(R))) - 1
  if(R < 1) {
    out <- c(U=U, N=NA_real_, N.L=NA_real_, N.U=NA_real_,
      V=NA_real_, CV=NA_real_)
  } else {
    out <- c(U=U, N=N, N.L=CI[1], N.U=CI[2], V=V, CV=CV)
  }
  return(out)
}
