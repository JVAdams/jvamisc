#' Recode Values
#'
#' Assign new values to a vector.  
#' @param x 			A vector whose values will be recoded, can be character, numeric, or factor.
#' @param old 			A vector of the unique values currently in the vector.
#' @param new 			A vector of values which should replace the current ones.
#' @param must.match 	A logical scalar indicating whether only those elements of the original vector with values in \code{old} should be returned (TRUE),
#' or all values should be returned (FALSE) though some may be unchanged, default TRUE.
#' @return 				A vector the same length as \code{x} (unless \code{must.match=TRUE}), with \code{old} values replaced by \code{new} values.
#' @export
#' @examples 
#' recode(c(1,1,1,2,3,4,1,10,3), 1:3, 1001:1003)
#' recode(c(1,1,1,2,3,4,1,10,3), 1:3, 1001:1003, must.match=FALSE)
recode <- function(x, old, new, must.match=TRUE) {
	partial <- match(x, old)
	if(must.match) new[partial] else ifelse(!is.na(partial), new[partial], x)
	}
