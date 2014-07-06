#' Quit
#'
#' Terminate the current R session.
#' @param save 	A character string indicating whether the workspace should be saved, "yes" (the default), "no", "ask" or "default".
#' @export
#' @seealso 	\code{\link{quit}}.

q <- function() quit(save="yes")
