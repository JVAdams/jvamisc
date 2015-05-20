#' Prettify the Numeric Columns of a Table
#'
#' Prettify the numeric columns of a table, by formating them and
#' converting them to character columns for printing.
#' @param df
#'   A data frame to be prettified.
#' @param digits
#'   Integer vector of either length 1 or the number of columns in \code{df},
#'   number of digits to be used, default 2.  See \code{round}.
#' @param rounds
#'   Logical vector of either length 1 or the number of columns in \code{df},
#'   indicating whether numbers should be rounded to \code{digits} decimal
#'   places (TRUE, the default), rounded to \code{digits} significant digits
#'   (FALSE), or not rounded at all (NULL).
#' @param bigseps
#'   Character vector of either length 1 or the number of columns in
#'   \code{df}, giving the character to be used as a mark between
#'   every three digits before the decimal, default ",".
#' @return
#'   A data frame the same dimensions as \code{df} with
#'   the numeric columns converted to character columns, formated as specified.
#' @export
#' @examples
#' head(mtcars)
#' prettytable(head(mtcars))
#'
prettytable <- function(df, digits=2, rounds=TRUE, bigseps=",") {
  lengthen <- function(y, n) {
    if (length(y)<2) rep(y, n) else y
  }
  formatnum <- function(x, dig., round., bigsep.) {
    if (is.integer(x) | is.numeric(x)) {
      decdig <- 0
      if (is.null(round.)) {
        x2 <- x
      } else {
        if (round.) {
          x2 <- round(x, dig.)
          decdig <- dig.
          decdig[decdig<0] <- 0
        } else {
          x2 <- signif(x, dig.)
        }
      }
      return(format(x2, big.mark=bigsep., nsmall=decdig))
    } else {
      return(x)
    }
  }
  N <- dim(df)[2]
  d <- lengthen(digits, N)
  r <- lengthen(rounds, N)
  b <- lengthen(bigseps, N)
  df2 <- df
  for(i in 1:N) {
    df2[, i] <- formatnum(x=df[, i], dig.=d[i], round.=r[i], bigsep.=b[i])
  }
  df2
}
