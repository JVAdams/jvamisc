#' Read Data Frame from Clipboard
#'
#' Read in a data frame from text pasted to the clipboard.
#' @param ... 	Additional parameters to \code{\link{read.table}}.
#' @return 		Data frame.
#' @export
#' @seealso		\code{\link{read.table}}.

dfclip <- function(...) read.table("clipboard", header=TRUE, ...)
