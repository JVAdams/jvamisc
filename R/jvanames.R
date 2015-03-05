#' Format Names
#'
#' Format names to be all lower case, unique, and without underscores.
#' @param charvec
#'   A character vector to be trimmed.
#' @return
#'   A character vector, the same length as \code{charvec}, but with
#'   all lower case, unique, and without underscores.
#' @export
#' @seealso
#'   \code{\link{make.names}}
#' @examples
#' jvanames(c("Conc_mgL", "Conc_mgL", "Temp"))

jvanames <- function(charvec) {
	make.names(casefold(charvec), unique=TRUE, allow_=FALSE)
}
