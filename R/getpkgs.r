#' Get Packages
#'
#' Installs (if necessary) and attaches the specified packages.
#' @param want 	A character vector of package names.
#' @export
#' @examples plotdf(mtcars)
getpackages <- function(want) {
	# install (if necessary) and attach wanted packages
	have <- row.names(installed.packages())
	need <- want[!(want %in% have)]
	if(length(need)>0) install.packages(need, repos="http://cran.r-project.org")
	lapply(want, require, character.only=TRUE)
	invisible()
	}
