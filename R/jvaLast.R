#' Shutdown
#'
#' One function with all the commands I typically want run at the end of an
#' R session.
#' @param file
#'   Character scalar, file in which to save the commands history relative to
#'   current working directory, default ".Rhistory".
#' @param nlines
#'   Integer scalar, number of lines that saved to commands history,
#'   default 10,000.
#' @export
#' @seealso
#'   \code{\link{savehistory}}, \code{\link{Sys.setenv}}.

jvaLast <- function(file=".Rhistory", nlines=10000) {
  if (interactive()) {
    Sys.setenv(R_HISTSIZE=nlines)
    cat("\nSession ended at ...............................", date(), "\n")
    try(savehistory(file))
  }
}
