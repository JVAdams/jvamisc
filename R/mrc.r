#' Estimates from Mark-Recapture Studies
#'
#' Estimates the recovery rate and abundance of a closed population from a mark-recapture study with a single capture and marking event and a single recapture event.  
#' @param m 	A numeric scalar, the number of individuals marked and released.
#' @param r 	A numeric scalar, the number of marked individuals recaptured.
#' @param c		A numeric scalar, the number of individuals checked for marks.
#' @param alpha A scalar, the desired confidence level, default \code{0.05}.
#' @return 		A named vector with the estimated recovery rate \code{U} with lower and upper 100*(1-\code{alpha})\% confidence limits from \code{\link{binomCI}},
#' and the number of observations \code{N} (the bias-corrected Petersen estimator from Chapman 1954 Table 3)
#' with lower and upper 100*(1-\code{alpha})\% confidence limits from the normal approximation of the Poisson for \code{r} (Ricker 1975).
#' @export
#' @seealso		\code{\link{binomCI}}
#' @examples
#' mrc(100, 10, 1000)
#' @references
#'
#' D. G. Chapman.  1954.
#'	The estimation of biological populations.  
#'	Annals of Mathematical Statistics 25:1-15.
#' \href{http://projecteuclid.org/euclid.aoms/1177728844}{[link]}.
#'
#' W. E. Ricker.  1975.
#'	Computation and interpretation of biological statistics of fish populations. 
#'	Fisheries Research Board of Canada Bulletin. 191.

mrc <- function(m, r, c, alpha=0.05) {
	U <- binomCI(r, m-r, alpha=alpha)[1:3]
	N <- (m+1) * (c+1) / (r+1) - 1
	CI <- (m+1) * (c+1) / (r+1+(c(1, -1)*qnorm(1-alpha/2)*sqrt(r))) - 1
	c(U=U, N=N, N.L=CI[1], N.U=CI[2])
	}
