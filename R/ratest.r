#' Ratio Estimation
#'
#' Ratio estimation.
#' @param num 	A numeric vector, the numerator of the ratio.
#' @param den 	A numeric vector, the denominator of the ratio.
#' @param adj 	A numeric scalar, an adjustment factor to be multiplied by the ratio, default 1.
#' @details		All records with a missing value in either the numberator or the denominator are omitted from calculations.
#' @return 		A named vector with the sum of the numerators, the sum of the denominators, the ratio of the sums,
#'	the number of nonmissing input pairs, and the standard deviation and 95% confidence interval of the ratio.
#' @export
#' @seealso 	\code{\link[survey]{svydesign}}, \code{\link[survey]{svyratio}}.
#' @import 		survey
#' @examples
#' # Size, weekly income, and food cost of 33 families from Cochran's (1977) Sampling Techniques.
#' familysize <- c(2, 3, 3, 5, 4, 7, 2, 4, 2, 5, 3, 6, 4, 4, 2, 5, 3, 4, 2, 4, 2, 
#' 	5, 3, 4, 7, 3, 3, 6, 2, 2, 6, 4, 2)
#' income <- c(62, 62, 87, 65, 58, 92, 88, 79, 83, 62, 63, 62, 60, 75, 90, 75, 
#' 	69, 83, 85, 73, 66, 58, 77, 69, 65, 77, 69, 95, 77, 69, 69, 67, 63)
#' foodcost <- c(14.3, 20.8, 22.7, 30.5, 41.2, 28.2, 24.2, 30, 24.2, 44.4, 13.4, 
#' 	19.8, 29.4, 27.1, 22.2, 37.7, 22.6, 36, 20.6, 27.7, 25.9, 23.5, 
#' 	39.8, 16.8, 37.8, 34.8, 28.7, 63, 19.5, 21.6, 18.2, 20.1, 20.7)
#' # weekly expenditure on food per person
#' ratest(foodcost, familysize)
#' # percentage of income spent on food
#' ratest(foodcost, income, 100)

ratest <- function(num, den, adj=1) {
	sel <- !is.na(num) & !is.na(den)
	sumnum <- sum(num[sel])
	sumden <- sum(den[sel])
	rat <- adj*sumnum/sumden
	n <- sum(sel)
	if(n < 2) {
		out <- c(sumnum=sumnum, sumden=sumden, rat=rat, n=n, sd=0, cilo=NA, cihi=NA)
		} else {
		mydesign <- svydesign(ids=~ 1, weights=1, data=data.frame(n=num[sel], d=den[sel]))
		myratio <- svyratio(~n, ~d, mydesign)
		um <- as.vector(unlist(myratio))
		ci <- as.vector(confint(myratio)[1, ])
		ci[1][ci[1]<0] <- 0
		out <- c(sumnum=sumnum, sumden=sumden, rat=rat, n=n, sd=adj*sqrt(um[2]), cilo=adj*ci[1], cihi=adj*ci[2])
		}
	out
	}
