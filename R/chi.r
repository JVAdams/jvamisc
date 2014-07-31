#' Chi-Squared Test
#'
#' Performs chi-squared contingency table tests with informative output.
#' @param x 	A numeric matrix.
#' @param rpct 	A numeric scalar indicating the rounding used for printed output, default 0.
#' @return 		A list with class "htest" containing the components described in \code{\link{chisq.test}}.
#' @export
#' @seealso 	\code{\link{chisq.test}}.
#' @examples 
#' ## From Agresti(2007) p.39
#' M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
#' dimnames(M) <- list(gender = c("M", "F"), party = c("Democrat", "Independent", "Republican"))
#' chi(M)
chi <- function(x, rpct=0) {
	sum1 <- apply(x, 1, sum)
	sum2 <- apply(x, 2, sum)
	exp <- outer(sum1, sum2)/sum(x) # contribution to chi square
	cont <- (x - exp)^2/exp
	cutoff <- qchisq(p=0.95, df=prod(dim(x)-1))
	cont.ord <- sort(as.vector(cont))
	major <- rev(cont.ord[cumsum(cont.ord)>cutoff])
	cat("\nExpected percentages\n")
	print(round((100 * exp)/apply(exp, 1, sum), rpct))
	cat("\nObserved percentages\n")
	print(round((100 * x)/sum1, rpct))
	cat("\nContribution to chi square\n")
	print(round(cont, 1))
	plot(sort(cont), pch = 16)
	cat("\nCutoff\n")
	print(cutoff)
	cat("\nMajor players\n")
	print(major)
	chisq.test(x)
}
