#' Stratified Random Sampling Estimates
#'
#' Calculates estimated means and variances from a stratified random
#' sampling design.
#' @param yhi
#'   A numeric vector of values obtained from the ith unit (in stratum h).
#' @param hi
#'   A numeric vector of stratum ids, the same length as yhi.
#' @param Wh
#'   A numeric vector of stratum weights, with length equal to the number
#'   of unique stratum ids (nh), ordered by stratum id.  If NULL (the default),
#'   weights are assumed to be equal for all strata (1/nh).
#' @param N
#'   Total of the quantities used for the stratum weights across all strata,
#'   used to expand estimated means to estimated totals.  If NULL (the
#'   default), no totals are estimated.
#' @return
#'   A named numeric vector of length 4, with the estimated mean and total,
#'   and their associated standard errors.
#' @export
#' @references
#'   Cochran, William G.  1977.
#'   Sampling Techniques.
#'   John Wiley & Sons, New York.
#' @examples
#' catch <- c(2, 4, 2, 4, 8, 9, 8, 9, 8, 9)
#' stratum <- c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2)
#' area <- c(3, 7)
#' stratCochran(yhi=catch, hi=stratum, Wh=area/sum(area), N=sum(area))
#' stratCochran(yhi=catch, hi=stratum)

stratCochran <- function(yhi, hi, Wh=NULL, N=NULL) {
	nh <- table(hi)
	ybarh <- tapply(yhi, hi, mean)
	s2h <- tapply(yhi, hi, var)
	if(is.null(Wh)) Wh <- rep(1/length(nh), length(nh))
	ybarst <- sum(Wh*ybarh)
	seybarst <- sqrt(sum(Wh^2*s2h/nh))
	if(!is.null(N)) {
		ytotst <- N*ybarst
		seytotst <- N*seybarst
	} else {
		N <- NA
		ytotst <- NA
		seytotst <- NA
	}
	c(ybarst=ybarst, seybarst=seybarst, ytotst=ytotst, seytotst=seytotst)
	}
