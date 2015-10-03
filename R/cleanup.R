#' Quick Clean Up
#'
#' Removes all objects from the current working directory.
#' @details
#'   User is prompted for response.  If the first letter of the response is "y" or "Y", all objects are removed from the current working directory.
#' @export

cleanup <- function() {
  answer <- readline("Do you want to remove all objects from .GlobalEnv? ")
  if (casefold(substring(answer, 1, 1)) == "y") {
    rm(list=ls(name=".GlobalEnv"), pos=".GlobalEnv")
    cat("All objects removed.\n")
  } else {
    cat("Okay, nevermind.\n")
  }
}
