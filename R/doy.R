#' Day of Year
#'
#' Calculates the day of the year.
#' @param date
#'   A vector of dates to be converted.
#' @param day1
#'   A character scalar specifying what date should equate to day 1,
#'   use "01-01", the default, to get the Julian day.
#' @return
#'   A numeric vector the same length as \code{date} giving the day of the year.
#' @export
#' @examples
#' x <- as.Date(c("1963-01-15", "1972-07-20", "1999-03-10"))
#' doy(x)
#' doy(x, "03-01")

doy <- function(date, day1="01-01") {
  jdaydefault <- julian(date)
  jdayorigin <- julian(as.Date(paste(lubridate::year(date), day1, sep="-")))
  as.numeric(jdaydefault - jdayorigin + 1)
}
