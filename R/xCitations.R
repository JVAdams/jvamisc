#' Extract Citations from Text
#'
#' Extract citations from text by searching for parenthetical phrases with
#' capital words (names) and 4-digit numbers (years)
#' @param file
#'   Character scalar indicating the name of the text file.  Default value of
#'   NULL allows for text to be read in directly using the \code{txt} argument.
#' @param txt
#'   Character scalar containing the text containing citations.  Default value
#'   of NULL expects text to be read in from a file using the \code{file}
#'   argument.
#' @details
#'   Citations in which the author is mentioned directly in the text and
#'   only the year is in parentheses are not captured with this function.
#' @return
#'   A character vector of unique citations in alphabetical order.
#' @export
#' @references
#'   Based on code posted by Kay Cichini on 26 Mar 2012 on theBioBucket
#'   \href{http://thebiobucket.blogspot.com/2012/03/how-to-extract-citation-from-body-of.html}{[link]}.
#' @import
#'   stringr
#' @examples
#' x <- paste("Yarmouth (1977) said something.",
#'  "Evidence of x (Barber and Jones 1991),",
#'  "y (House et al. 1982),",
#'  "and z (Smith 1990; Folger and Penn 2000).")
#' xCitations(txt=x)
#'
xCitations <- function(file=NULL, txt=NULL) {
  if(is.null(txt)) txt <- readLines(file)
  # retrieve text inbetween parantheses:
  x1 <- unlist(str_extract_all(txt, pattern = "\\(.*?\\)"))
  # extract partial strings starting with uppercase letter (name)
  # and end in a four digit string (year):
  # each citation seperately:
  x <- unlist(str_extract_all(x1, "[A-Z].*?[0-9]{4}"))
  return(sort(unique(x)))
}
