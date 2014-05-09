#' Quit
#'
#' Quit R
#' @param save a character string indicating whether the workspace should be saved, "yes" (the default), "no", "ask" or "default"
#' @param ... other arguments to \code\link{{quit}}
#' @return the R session is terminated (nothing is returned)
#' @export
#' @seealso \code\link{{quit}}

q <- function() quit(save="yes", ...)
