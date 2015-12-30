#' Document a Package
#'
#' Document a package from local files.
#' \pkg{devtools} package required.
#' @param package
#'   A character scalar, package name.
#' @param dir
#'   A character scalar, directory where package folder is located,
#'     default "C:/JVA/GitHub/".
#' @export
#' @seealso
#'   \code{\link{pkgin}}

pkgup <- function(package, dir="C:/JVA/GitHub/") {
  if (!requireNamespace("devtools", quietly=TRUE)) {
    stop("devtools must be installed.", call.=FALSE)
  }
  setwd(paste0(dir, package))
  devtools::document()
  }
