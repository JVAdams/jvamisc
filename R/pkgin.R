#' Install a Package
#'
#' Install a package from local files and load the library.
#' \pkg{devtools} package required.
#' @param package
#'   A character scalar, package name.
#' @param ld
#'   A character scalar, current local directory, default \code{getwd()}.
#' @param pd
#'   A character scalar, R package directory, default "C:/JVA/GitHub".
#' @export
#' @seealso
#'   \code{\link{pkgup}}

pkgin <- function(package, ld=getwd(), pd="C:/JVA/GitHub") {
  if (!requireNamespace("devtools", quietly=TRUE)) {
    stop("devtools must be installed.", call.=FALSE)
  }
  # install from local folder
  setwd(pd)
  devtools::install(package)
  setwd(ld)
}
