#' Plot a Matrix as a Table with Colored Cells
#'
#' Plot a matrix as a table with colored cells.
#' @param mvalue
#'   Numeric matrix used to generate colors of table.
#' @param mtext
#'   Character or numeric matrix to display in the table, default \code{mvalue}.
#' @param ctitle
#'   Character scalar, column title, default "Columns".
#' @param rtitle
#'   Character scalar, row title, default "Rows".
#' @param vcex
#'   Numeric scalar, character expansion for the table values, default 1.
#' @param lcex
#'   Numeric scalar, character expansion for the column and row axis labels,
#'   default 1.
#' @param mcex
#'   Numeric scalar, character expansion for the column and row axis titles,
#'   default 1.
#' @param ctline
#'   Numeric scalar, number of lines from upper margin to display column title,
#'   default 3.
#' @param rtline
#'   Numeric scalar, number of lines from left margin to display row title,
#'   default 4.
#' @param colorange
#'   Vector of length 2 giving the color extremes to display for the lowest
#'   and highest values in \code{mvalue}, default c("white", "magenta").
#' @param mrange
#'   Numeric vector of length 2, an explicit value range corresponding to
#'   \code{colorange}.  When set to NULL, the default, the \code{colorange}
#'   corresponds to the range of values in \code{mvalue}.
#' @param border
#'   Scalar or matrix of colors to be useed for the cell borders, default NA
#'   gives no border.
#' @param ...
#'   Additional parameters to \code{\link[plotrix]{color2D.matplot}}.
#' @details
#'   Displays a plot with the same number of rectangular cells as there are
#'   values in the matrix. Each rectangle is colored to represent its
#'   corresponding value. The rectangles are arranged in the conventional display
#'   of a matrix with rows beginning at the top and columns at the left.
#' @export
#' @seealso
#'   \code{\link[plotrix]{color2D.matplot}}
#' @references
#'   Based on a method posted by Henrik on 6 September 2013 on stackoverflow
#'   \href{http://stackoverflow.com/a/18665994/2140956}{[link]}.
#' @import
#'   plotrix
#' @examples
#' m <- matrix(c(6, 5, 7, 20, 30, 5, 5, 10:14), nrow=3,
#'  dimnames=list(c("a", "b", "c"), c("W", "X", "Y", "Z")))
#' plotmatrix(m)
#' plotmatrix(sqrt(m), m)
#' plotmatrix(m, mrange=c(0, 50))

plotmatrix <- function(mvalue, mtext=mvalue, ctitle="Columns", rtitle="Rows",
  vcex=1, lcex=1, mcex=1, ctline=3, rtline=4, colorange=c("white", "magenta"),
  mrange=NULL, border=NA, ...) {
  if(is.null(mrange)) {
    cc <- array(colr(mvalue, colorange[1], colorange[2]), dim=dim(mvalue))
  } else {
    vcolz <- colr(c(mrange, as.vector(mvalue)), colorange[1], colorange[2])
    cc <- array(vcolz[-(1:2)], dim=dim(mvalue))
  }
  color2D.matplot(mtext, show.values=TRUE, axes=FALSE, cellcolors=cc,
    vcex=vcex, xlab="", ylab="", border=border, ...)
  axis(3, at=seq_len(ncol(mtext)) - 0.5,
    labels=colnames(mtext), tick=FALSE, cex.axis=lcex)
  mtext(ctitle, 3, cex=mcex, line=ctline)
  axis(2, at=seq_len(nrow(mtext)) - 0.5,
    labels=rev(rownames(mtext)), tick=FALSE, las=1, cex.axis=lcex)
  mtext(rtitle, 2, cex=mcex, line=rtline)
  box(col=0)
}
