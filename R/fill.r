#' Fill in Missing Values
#'
#' Fill in missing values in a vector, using the last recorded value.  
#' @param x 	A vector, can be character, numeric, or logical.
#' @return 		A vector the same length as \code{x}, with all NAs or ""s replace by the last value for the vector.
#' Note that and missing values at the beginning of the vector will not be replaced.
#' @export
#' @references Thomas A. Louis (1981). Confidence intervals for a binomial parameter after observing no successes. 
#' The American Statistician, 35(3), 154.
#' \url{http://amstat.tandfonline.com/doi/abs/10.1080/00031305.1981.10479337?journalCode=utas20#.U3t2EvldX64}.
#' @examples 
#' numvec <- c(NA, 1:5, NA, NA, NA, 10:12, NA)
#' fill(numvec)
#'
#' charvec <- c("", letters[1:5], "", "", "", letters[10:12], "")
#' fill(charvec)

fill <- function(x)
	{
	# fill in a vector of values
	# assign to every NA or "" the last value for the vector
    y <- x
    last <- x[1]
    if(is.character(x)){
        for(i in 2:length(x))
            if(x[i]=="" | is.na(x[i])) y[i] <- last else last <- x[i]
        } else {
        for(i in 2:length(x))
            if(is.na(x[i])) y[i] <- last else last <- x[i]
        }
    y
	}

