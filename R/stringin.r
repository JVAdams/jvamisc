#' Find a String in a Character Vector
#'
#' Find a string in a character vector.
#' @param pattern		Character scalar, string to be matched in \code{x}.
#' @param x				Character vector where matches are sought.
#' @param ignore.case	Logical scalar, indicating case sensitivity, default TRUE.
#' @param value			Logical scalar, indicating whether to return the matching elements (the default, TRUE) or their indices (FALSE).
#' @param fixed			Logical scalar, indicating whether string should be matched as is, default TRUE.
#' @param ...			Other arguments to \code{\link{grep}}.
#' @return 				Character vector containing the matching elements of \code{x}.
#' @export
#' @seealso 			\code{\link{grep}}.
#' @examples
#' txt <- c("The", "licenses", "for", "most", "software", "are", "designed", "to", "take", "away", "your", "freedom", "to", "share", "and", "change", "it.", "", "By", "contrast,", "the", "GNU", "General", "Public", "License", "is", "intended", "to", "guarantee", "your", "freedom", "to", "share", "and", "change", "free", "software", "--", "to", "make", "sure", "the", "software", "is", "free", "for", "all", "its", "users.")
#' stringin("b", txt)
#' stringin("ar", txt)

stringin <- function(pattern, x, ignore.case=TRUE, value=TRUE, fixed=TRUE, ...) {
	grep(pattern, x, ignore.case=ignore.case, value=value, fixed=fixed, ...)
	}
