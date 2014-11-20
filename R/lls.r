#' List All Members of an Environment.
#'
#' Generate a descriptive list of all members of an environment.
#' @param pos 	A scalar of the environment, search path index, or package of interest, default 1.
#' @param pat 	Regular expression pattern specifying which members to return, default "" for all members.
#' @return 		A data frame with the name, class, dimension, and size of each member of the environment.
#' @export
#' @references 	Based on a method posted by Bendix Carstensen on 10 Jan 2007 on R-help 
#' \href{https://stat.ethz.ch/pipermail/r-help/2007-January/123403.html}{[link]}.
#' @examples
#' \dontrun{
#' lls()
#' }

lls <- function(pos=1, pat="") {
	# modification of original function
	# http://tolstoy.newcastle.edu.au/R/e2/help/07/01/8286.html
	# Bendix Carstensen - 10 Jan 2007
	# see also R.oo ll() function
	dimx <- function(dd) if(is.null(dim(dd))) length(dd) else dim(dd)
	lll <- ls(pos=pos, pattern=pat)
	if(length(lll) > 0) {
		x <- data.frame(array(dim=c(length(lll), 3), dimnames=list(lll, c("class", "ndim", "size"))))
		y <- rep(NA, length(lll))
		for(i in 1:length(lll)) {
				x$class[i] <- paste(eval(parse(text=paste("class(", lll[i], ")"))), collapse=" ")
				x$ndim[i] <- length(eval(parse(text=paste("dimx(", lll[i], ")"))))
				x$size[i] <- paste(eval(parse(text=paste("dimx(", lll[i], ")"))), collapse=" x ")
				y[i] <- prod(eval(parse(text=paste("dimx(", lll[i], ")"))))
			}
		}
	x$ndim[x$class=="function"] <- 0 
	x[order(-x$ndim, -y, x$class), ]
	}
