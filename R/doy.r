#' Day of Year
#'
#' Calculates the day of the year.  
#' @param date 	A vector of dates to be converted.
#' @param day1 	A character scalar specifying what date should equate to day 1, use "01-01", the default, to get the Julian day.
#' @return 		A numeric vector the same length as \code{date} giving the day of the year.
#' @import		lubridate
#' @export
#' @examples 
#' x <- as.Date(c("1963-01-15", "1972-07-20", "1999-03-10"))
#' doy(x)
#' doy(x, "03-01")

doy <- function(date, day1="01-01") {
	sapply(1:length(date), function(i) as.numeric(julian(date[i], origin=as.Date(paste(year(date[i]), day1, sep="-"))) + 1))
	}
