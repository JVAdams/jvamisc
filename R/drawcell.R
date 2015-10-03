#' Draw Table Cell
#'
#' Draw a table cell at location (r,c).
#' @param title
#'   A character scalar, the text to be placed in the table cell.
#' @param r
#'   An integer scalar, the row number of the table cell.
#' @param c
#'   An integer scalar, the column number of the table cell.
#' @param text.cex
#'   A numeric scalar, the relative size of the text to be place in
#'   the table cell, default 1.
#' @param bg.col
#'   A character or numeric scalar, indicating the background color to use
#'   for the table cell, default "white".
#' @param frame.cell
#'   A logical scalar, indicating whether a black border should be drawn
#'   around the table cell, default TRUE.
#' @return
#'   A table cell is added to the current table.
#' @export
#' @details
#'   This is part of group of function to plot tables, modifications of
#'   functions in the Systmatic Investor Toolbox.
#' @references
#'   Systmatic Investor Toolbox.
#'   \emph{https://github.com/systematicinvestor/SIT}

drawcell <- function(title, r, c, text.cex=1,
  bg.col="white", frame.cell=TRUE) {
  if (!frame.cell) {
    bcol=bg.col
  } else {
    bcol='black'
  }
  rect((2*(c - 1) + .5), -(r - .5), (2*c + .5), -(r + .5), col=bg.col,
    border=bcol)
  if (c==1) {
    text((2*(c - 1) + .5), -r, title, adj=0, cex=text.cex)
  } else {
    if (r==1 ) {
      text((2*(c - 1) + .5), -r, title, adj=0, cex=text.cex)
    } else {
      text((2*c + .5), -r, title, adj=1, cex=text.cex)
    }
  }
}
