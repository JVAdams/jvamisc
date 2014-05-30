#' Quit
#'
#' Quit R
#' @param save 	A character string indicating whether the workspace should be saved, "yes" (the default), "no", "ask" or "default".
#' @return 		The R session is terminated (nothing is returned).
#' @export
#' @seealso 	\code\link{{quit}}.

q <- function() quit(save="yes")
