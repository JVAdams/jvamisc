#' @name
#'   mapL
#' @title
#'   Great Lakes Shorelines
#' @description
#'   List of five data frames, one for each Great Lake
#'   (Superior, Michigan, Huron, Erie, Ontario)
#'   with longitude and latitude coordinates of the shorelines.
#' @format
#'   One row for each point, consecutive rows with non-missing values for
#'   each line.
#' @source
#'   NOAA National Geophysical Data Center, (the now defunct) Coastline
#'	 Extractor,
#'   \href{http://www.ngdc.noaa.gov/mgg/coast/}{ngdc.noaa.gov/mgg/coast}.
#' @author
#'   Rich Signell, USGS.
NULL
#' @name
#'   map5
#' @title
#'   Great Lakes Basin Shoreline
#' @description
#'   A single data frame with longitude and latitude coordinates
#'   of the shorelines in the Great Lakes basin.
#' @format
#'   One row for each point, consecutive rows with non-missing values for
#'   each line.
#' @source
#'   NOAA National Geophysical Data Center, (the now defunct) Coastline
#'	 Extractor,
#'   \href{http://www.ngdc.noaa.gov/mgg/coast/}{ngdc.noaa.gov/mgg/coast}.
#' @author
#'   Rich Signell, USGS.
NULL
#' @name
#'   mapSMR
#' @title
#'   St. Marys River Shoreline
#' @description
#'   Data frame with longitude and latitude coordinates
#'   of the St. Marys River shoreline (the connecting channel between
#'   Lakes Superior and Huron in the Great Lakes basin).
#' @format
#'   One row for each point, consecutive rows with non-missing values for
#'   each line.
#' @source
#'   NOAA National Geophysical Data Center, (the now defunct) Coastline
#'	 Extractor,
#'   \href{http://www.ngdc.noaa.gov/mgg/coast/}{ngdc.noaa.gov/mgg/coast}.
#' @author
#'   Rich Signell, USGS.
NULL
#' @name
#'   Lakenames
#' @title
#'   Great Lakes Names
#' @description
#'   A vector with the names of the five Great Lakes.
#' @format
#'   A character vector, length 5.
NULL
#' @name
#'   Lakeabbs
#' @title
#'   Great Lakes Abbreviations
#' @description
#'   A vector with the first initials of the five Great Lakes.
#' @format
#'   A character vector, length 5.
NULL
#' @name
#'   blindcolz
#' @title
#'   Color-blind Friendly Colors
#' @description
#'   A vector with nine color-blind friendly colors in hex code.
#' @format
#'   A character vector, length 9.
#' @source
#'   How to make figures and presentations that are friendly to color blind
#'   people,
#'   20 November 2002,
#'   \href{http://jfly.iam.u-tokyo.ac.jp/html/color_blind/}{[link]}.
#' @author
#'   Masataka Okabe and Kei Ito.
NULL
#' @name
#'   states
#' @title
#'   United States State Boundaries Map
#' @description
#'   Geographical data base for the state boundaries of the US,
#'   generated from US Department of the Census data.
#' @format
#'   A list (of class map) with elements x, y, range, and names.
#'   The x and y vectors have longitude (x) and latitude (y) coordinates of
#'   successive state polygons, separated by NAs.
#' @source
#'   A copy of the \code{state} data base from the \code{maps} package.
NULL
#' @name
#'   counties
#' @title
#'   United States County Boundaries Map
#' @description
#'   Geographical data base for the county boundaries of the US,
#'   generated from US Department of the Census data.
#' @format
#'   A list (of class map) with elements x, y, range, and names.
#'   The x and y vectors have longitude (x) and latitude (y) coordinates of
#'   successive county polygons, separated by NAs.
#' @source
#'   A copy of the \code{county} data base from the \code{maps} package.
NULL
#' @name
#'   jvamiscenv
#' @title
#'   jvamisc Package Local Environment
#' @description
#'   An environment local to the jvamisc package, used to hold objects
#'   outside of the individual package functions
#' @format
#'   An environment.
#' @source
#'   Post from Hadley Wickham to r-help on 2 Dec 2014
#'	\href{https://stat.ethz.ch/pipermail/r-help/2014-December/423847.html}{[link]}.
NULL
