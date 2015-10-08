#' Stratified Random Sampling Estimates
#'
#' Calculates estimated means and variances from a stratified random
#' sampling design.
#' @param yhi
#'   A numeric vector of values obtained from the ith unit (in stratum h).
#' @param hi
#'   A vector of stratum ids, the same length as \code{yhi}.
#' @param uh
#'   A vector of unique stratum ids.
#' @param Wh
#'   A numeric vector of stratum weights, the same length as and in the same
#'   order as \code{uh}.  By default,
#'   equal weights are assumed for all strata \code{(1/length(uh))}.
#'   Need not sum to one, the input values will be adjusted automatically
#'   to do so.
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

stratCochran <- function(yhi, hi, uh=sort(unique(hi)),
  Wh=rep(1/length(uh), length(uh)), N=NULL) {
  if(length(yhi) != length(hi)) stop(
    "yhi and hi should be the same length")
  nh <- table(hi)
  ybarh <- tapply(yhi, hi, mean)
  s2h <- tapply(yhi, hi, var)
  if(length(nh) != length(uh)) stop(
    "The number of unique values in hi should be equal to the length of uh")
  if(length(nh) != length(Wh)) stop(
    "The number of unique values in hi should be equal to the length of Wh")
  if(length(uh) != length(Wh)) stop(
    "uh and Wh should be the same length")
  ord <- order(uh)
  Wh.o <- Wh[ord]/sum(Wh)
  ybarst <- sum(Wh.o*ybarh)
  seybarst <- sqrt(sum(Wh.o^2*s2h/nh))
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
