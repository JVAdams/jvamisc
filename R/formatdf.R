#' Format a Data Frame
#'
#' Format selected columns of a data frame for pretty viewing or printing.
#' @param df
#'   Data frame to be formatted.
#' @param numercol
#'   Numeric vector, index of numeric columns to format, default, NULL,
#'   uses all numeric and integer columns.
#' @param round2
#'   Integer vector, indicating what place numbers should be rounded to,
#'   default 0.
#' @param ndec
#'   Integer vector, indicating how many places to the right of
#'   the decimal point should be displayed, default \code{round2}.
#' @param comma
#'   Character vector, indicating thousands character separator, default ",".
#' @param characol
#'   Numeric vector, index of character columns to format, default, NULL,
#'   uses all character and factor columns.
#' @param align
#'   Character vector, indicating justification of character columns,
#'   default "left". Other options include "right", "centre", and "none".
#' @param keepnum
#'   Logical scalar, indicating if numeric columns should be kept as numeric,
#'   default TRUE, or converted to character.
#' @return
#'   A data frame, the same dimensions as \code{df} with selected columns
#'   rounded and/or formatted and converted to character.
#' @details
#'   If a vector of length 1 is provided for \code{round2}, \code{ndec},
#'   \code{comma}, or \code{align}, the same rounding, decimals, commas, or
#'   alignment is applied to all specified columns.
#' @seealso
#'   \code{\link{format}}.
#' @export
#' @examples
#' head(mtcars)
#' formatdf(head(mtcars))

formatdf <- function(df, numercol=NULL, round2=0, ndec=round2, comma=",",
  characol=NULL, align="left", keepnum=TRUE) {
  dfclass <- sapply(df, class)
  df2 <- df
  numercol <- if (is.null(numercol)) {
    dfclass %in% c("numeric", "integer")
  }
  characol <- if (is.null(characol)) {
    dfclass %in% c("character", "factor")
  }
  nn <- sum(numercol)
  if (nn > 0) {
    colz <- (1:dim(df)[2])[numercol]
    for(j in 1:nn) {
      i <- colz[j]
      if (keepnum) {
        df2[, i] <- round(df[, i], rep(round2, length=nn)[j])
      } else {
        df2[, i] <- format(round(df[, i], rep(round2, length=nn)[j]), nsmall=rep(ndec, length=nn)[j], big.mark=rep(comma, length=nn)[j])
      }
    }
  }
  nc <- sum(characol)
  if (nc > 0) {
    colz <- (1:dim(df)[2])[characol]
    for(j in 1:nc) {
      i <- colz[j]
      y <- if (dfclass[i]=="factor") {
        as.character(df[, i])
      } else {
        df[, i]
      }
      df2[, i] <- format(y, justify=rep(align, length=nc)[j])
    }
  }
  df2
}
