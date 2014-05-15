#' Data frame from clipboard
#'
#' Read in a data frame from headers/rows pasted to the clipboard.
#' @param ... parameters to \link{\code{read.table}}
#' @return data frame 
#' @export

dfclip <- function(...) read.table("clipboard", header=TRUE, ...)
