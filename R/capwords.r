#' Capitalize
#'
#' Capitalize the first letter of every word.
#' @param s 		A vector of strings.
#' @param strict 	A logical indicating whether other letters should be converted to lower case, default TRUE.
#' @return 			A vector the same length as \code{s}.
#' @export
#' @seealso 		\code{\link{casefold}}, from which the function was derived.
#' @examples
#' capwords(c("using AIC for model selection"))
#' capwords(c("using AIC", "for MODEL selection"), strict=FALSE)

capwords <- function(s, strict=TRUE) {
	# function from help file on casefold()
    cap <- function(s) paste(toupper(substring(s,1,1)),
                  {s <- substring(s,2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
