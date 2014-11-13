#' Copy a Package Manual
#'
#' Copy a package reference manual created by R CMD Check to the corresponding GitHub folder.
#' @param package 	A character scalar, package name.
#' @param file 		A character scalar, path of file to be copied, default 
#'	paste0("C:/Users/jvadams/", package, ".Rcheck/", package, "-manual.pdf").
#' @param dir 		A character scalar, directory where package folder is located, default "C:/JVA/GitHub/".
#' @export
#' @seealso			\code{\link{pkgin}}

pkgman <- function(package, file=paste0("C:/Users/jvadams/", package, ".Rcheck/", package, "-manual.pdf"), dir="C:/JVA/GitHub/") {
	file.copy(file, paste0(dir, package), overwrite=TRUE)
	}
