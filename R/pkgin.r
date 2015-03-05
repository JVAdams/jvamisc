#' Install a Package
#'
#' Install a package from local files, load the library, and
#' save installed package to zip archive.
#' @param package
#'   A character scalar, package name.
#' @param wd
#'   A character scalar, R working directory,
#'   default "C:/JVA/R/Working Directory".
#' @param ld
#'   A character scalar, R library directory,
#'   default "C:/Users/jvadams/Documents/R/win-library/3.1".
#' @param pd
#'   A character scalar, R package directory, default "C:/JVA/GitHub".
#' @import
#'   devtools
#' @export
#' @seealso
#'   \code{\link{pkgup}}

pkgin <- function(package, wd="C:/JVA/R/Working Directory",
    ld="C:/Users/jvadams/Documents/R/win-library/3.1", pd="C:/JVA/GitHub") {
	# install from local folder
	setwd(pd)
	install(package)
	# set the library folder as the working directory
	setwd(ld)
	# save installed package to zip archive
	zip(paste0(pd, "/", package, "/", package, ".zip"), package)
}
