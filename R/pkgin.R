#' Install a Package
#'
#' Install a package from local files and load the library.
#' \pkg{devtools} package required.
#' @param package
#'   A character scalar, package name.
#' @param ld
#'   A character scalar, R library directory,
#'   default "C:/Users/jvadams/Documents/R/win-library/3.2".
#' @param lv
#'   A character scalar, R library directory version number,
#'   default "max", which uses the latest version in \code{ld}.
#' @param pd
#'   A character scalar, R package directory, default "C:/JVA/GitHub".
#' @export
#' @seealso
#'   \code{\link{pkgup}}

pkgin <- function(package, ld="C:/Users/jvadams/Documents/R/win-library",
  lv="max", pd="C:/JVA/GitHub") {
  if (!requireNamespace("devtools", quietly=TRUE)) {
    stop("devtools must be installed.", call.=FALSE)
  }
  # install from local folder
  setwd(pd)
  devtools::install(package)
  # set the library folder as the working directory
  if (lv=="max") {
    lv <- max(list.files(ld))
  }
  setwd(paste(ld, lv, sep="/"))
}
