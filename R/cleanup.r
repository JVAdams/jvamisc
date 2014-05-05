#' Quick clean up
#'
#' Removes all objects from the current working directory.
#' @return a plot is sent to the current graphics device (no value is returned)
#' @export

cleanup <- function() {
	answer <- readline("Do you want to remove all objects from .GlobalEnv? ")
	if (casefold(substring(answer, 1, 1)) == "y") {
		rm(list=ls(name=".GlobalEnv"), pos=".GlobalEnv")
		cat("All objects removed.\n")
		} else cat("Okay, nevermind.\n")
}
