#' Convert Character to Numeric
#'
#' Convert a character vector to a numeric vector, save any non-numeric values to a separate vector of comments.
#' @param x 		A character vector to be converted to numeric.
#' @param varname	A character scalar to be used as an identifier in the generated comment vector.  If NULL (default), the name of x will be used.  If FALSE, no varname will be used in the comment.
#' @param pmissing	A character vector of all possible values to be interpreted as missings (and, therefore, not be commented upon).
#' @param justindex	A logical scalar indicating if the output should just be the index of non-numeric values, default FALSE.
#' @return 			By default, a list of length two with the converted numeric vector and the comment vector.
#'	If justindex = TRUE, a character vector the same length as x, indicating the elements with non-numeric values.
#' @export
#' @examples 
#'
#' a <- c(">1.5", "3", "NA", "missing", ".", "12")
#' char2num(a)
#' char2num(a, FALSE)
#' char2num(a, "pH")
#' char2num(a, pmissing=c("NA", "missing", "."))
#' char2num(a, justindex=TRUE)
#'
#' df <- data.frame(
#' 	x1 = c(NA, 8.2, 8.2, 8.2, 8.6, 8.1, 8.2), 
#' 	x2 = c(NA, "83", "*", "83", "79-80", "NA", "83"), 
#'     x3 = c(NA, NA, NA, "9.4", "?", ">10", "6.6"))
#' vars <- c("x2", "x3")
#' tonum <- lapply(vars, function(v) char2num(df[, v], varname=v))
#' df2 <- df
#' df2[, vars] <- data.frame(sapply(tonum, "[", 1))
#' df2$comment <- apply(do.call(cbind, sapply(tonum, "[", 2)), 1, paste, collapse="")

char2num <- function(x, varname=NULL, pmissing=c("NA", ".", "", " ", "-"), justindex=FALSE) {
	origopt <- options("warn")
	options(warn=-1)
	asn <- as.numeric(x)
	options(warn=origopt$warn)
	sel <- is.na(asn) & !is.na(x) & !x%in%pmissing
	if(justindex) {
		return(sel)
		} else {
		note <- rep("", length(x))
		if(is.null(varname)) varname <- deparse(substitute(x))
		if(varname==FALSE) {
			note[sel] <- x[sel]
			out <- list(asn, note)
			} else {
			note[sel] <- paste0(varname, ":", x[sel])
			out <- list(asn, note)
			names(out) <- c(varname, "note")
			}
		return(out)
		}
	}
