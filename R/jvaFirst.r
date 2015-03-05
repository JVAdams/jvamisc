#' Startup
#'
#' One function with all the commands I typically want run at the start of
#' an R session.
#' @param maxp
#'   Integer scalar, maximum number of lines printed, default 500.
#' @param ndec
#'   Integer scalar, maximum width of fixed notation before switching to
#'   scientific notation, default 10.
#' @param cont
#'   Character scalar, prompt used for lines which continue past
#'   first command line, default \code{"... "}.
#' @param pkgs
#'   Character vector, packages to be loaded, default c("rJava", "XLConnect",
#'   "maps", "mapproj", "RColorBrewer", "mgcv", "jvamisc").
#' @param mirror
#'   Character scalar, CRAN mirror, default
#'   "http://streaming.stat.iastate.edu/CRAN/".
#' @param helpt
#'   Character scalar, type of help, default "html".
#' @param fac
#'   Logical scalar, use factors rather than character strings, default FALSE.
#' @param noplots
#'   Logical scalar, remove "saved" plots, e.g., from past runs of
#'   \code{\link[jvamisc]{plotdf}}, default TRUE.
#' @param show
#'   Logical scalar, list objects in current environment, default TRUE.
#' @export
#' @seealso
#'   \code{\link{options}}, \code{\link{help}}.
#' @examples
#' jvaFirst()
jvaFirst <- function(maxp=500, ndec=10, cont="... ",
	  pkgs=c("rJava", "XLConnect", "maps", "mapproj", "RColorBrewer", "mgcv",
      "MASS", "jvamisc"),
	  mirror="http://streaming.stat.iastate.edu/CRAN/",
	  helpt="html", fac=FALSE, noplots=TRUE, show=TRUE) {

	# don't print more than maxp rows
	options(max.print=maxp)

	# prefer long decimals rather than scientific notation
	options(scipen=ndec)

	# make it more obvious when line of code is continued on the next line
	# from Tony Fischetti,
  #   http://www.onthelambda.com/2014/09/17/fun-with-rprofile-and-customizing-r-startup/
	options(continue=cont)

	# attach packages
	sshhh <- function(a.package) {
    suppressWarnings(suppressPackageStartupMessages(
      library(a.package, character.only=TRUE)))
	}
	sapply(pkgs, sshhh)

	# set CRAN mirror
	repo <- getOption("repos")
	repo["CRAN"] <- mirror
	options(repos=repo)

	# prefer compiled HTML help
	options(help_type=helpt)

	# prefer characters rather than factors
	options(stringsAsFactors=fac)

	# get rid of any "saved" plots, e.g., from past runs of dfplot()
	if (noplots) {
    .SavedPlots <- NULL
	}

	# my personal functions
	if (show) {
		already <- ls(".GlobalEnv")
		print(already)
	}
}
