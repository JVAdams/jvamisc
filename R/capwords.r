#' Capitalize
#'
#' Capitalize the first letter of every word.
#' @param s a vector of strings
#' @param strict a logical indicating whether other letters should be converted to lower case
#' @return a vector the same length as \code{s}
#' @export
#' @seealso \code{\link{casefold}} from which the function was derived
#' @examples
#' capwords(c("using AIC for model selection"))
#' ## ->  [1] "Using AIC For Model Selection"
#' capwords(c("using AIC", "for MODEL selection"), strict = TRUE)
#' ## ->  [1] "Using Aic"  "For Model Selection"
#' ##                ^^^        ^^^^^
#' ##               'bad'       'good'

capwords <- function(s, strict = FALSE) {
	# function from help file on casefold()
    cap <- function(s) paste(toupper(substring(s,1,1)),
                  {s <- substring(s,2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
