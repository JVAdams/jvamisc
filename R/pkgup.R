#' Document a Package
#'
#' Document a package from local files.
#' @param package
#'   A character scalar, package name.
#' @param dir
#'   A character scalar, directory where package folder is located,
#'     default "C:/JVA/GitHub/".
#' @import
#' devtools
#' @export
#' @seealso
#'   \code{\link{pkgin}}

pkgup <- function(package, dir="C:/JVA/GitHub/") {
  setwd(paste0(dir, package))
  document()
  }
