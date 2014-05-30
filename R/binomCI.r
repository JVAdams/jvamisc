#' Binomial Confidence Interval
#'
#' Calculates the binomial confidence interval from a sample, using the normal approximation.  
#' Uses Louis (1981) if only failures or only successes were observed.
#' @param x 	A vector of 0s and 1s, or (if \code{y} is also given) a scalar, the number of failures.
#' @param y 	A scalar, the number of successes, default \code{NULL}.
#' @param na.rm A logical, whether to remove NAs from the data, default \code{TRUE}.
#' @param alpha A scalar, the desired confidence level, default \code{0.05}.
#' @param prob 	A logical, whether to output results as probabilities, default \code{TRUE}, or counts.
#' @return 		A named vector with the \code{Mean}, lower and upper confidence limits (\code{L} and \code{U}), 
#' and the number of observations \code{N}.
#' @export
#' @references Thomas A. Louis. 1981. Confidence intervals for a binomial parameter after observing no successes. 
#' The American Statistician 35(3):154.
#' \url{http://amstat.tandfonline.com/doi/abs/10.1080/00031305.1981.10479337?journalCode=utas20#.U3t2EvldX64}.
#' @examples 
#' binomCI(c(0, 0, 0, 0, 1, 1))
#' binomCI(4, 2, prob=FALSE)

binomCI <- function(x, y=NULL, na.rm=TRUE, alpha=0.05, prob=TRUE) {
	# calculate mean proportion of successes with confidence limits
	# if only x is given, it is assumed to be a vector of 0s and 1s
	# if x and y are given, they are assumed to be counts of successes and failures 
	if(!is.null(y)) {
		if(is.na(x) | is.na(y)) z1 <- NA else z1 <- rep(1:0, c(x, y))
		} else {
		z1 <- x
		}
	if(na.rm) z1 <- z1[!is.na(z1)]
	n <- length(z1)
	if(n==0) {
		out <- c(Mean=NA, L=NA, U=NA, N=n)
		} else {
		xbar <- sum(z1)/n
		if(xbar==0) {
			out1 <- c(Mean=xbar, L=0, U=1 - alpha^(1/n), N=n)
			} else {
			if(xbar==1) {
				out1 <- c(Mean=xbar, L=alpha^(1/n), U=1, N=n)
				} else {
				sd <- sqrt(xbar*(1-xbar)/n)
				ci <- qnorm(1 - alpha/2)*sd
				lower <- if(ci>xbar) 0 else xbar-ci
				upper <- if(ci>(1-xbar)) 1 else xbar+ci
				out1 <- c(Mean=xbar, L=lower, U=upper, N=n)
				}
			}
		if(prob) {
			out <- out1
			} else {
			out <- c(out1[1:3]*n, out1[4])
			}
		}
	out
	}
