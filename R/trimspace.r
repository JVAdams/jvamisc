#' Trim Whitespace
#'
#' Trim leading and trailing whitespace from a character vector.
#' @param charvec
#'   A character vector to be trimmed.
#' @return
#'   A character vector, the same length as \code{charvec},
#'   without leading and trailing whitespace.
#' @export
#' @examples
#' trimspace("    Bob   and Alice")
#' trimspace(" Harriet and    June    ")

trimspace <- function(charvec) {
	gsub("(^ +)|( +$)", "", charvec)
}
