#' Convert Lat/Long to UTM
#'
#' Convert longitude and latitude coordinates to UTM easting and northing
#' coordinates.
#' @param lon
#'   Numeric vector of longitudes.
#' @param lat
#'   Numeric vector of latitudes, same length as \code{lon}.
#' @param zone
#'   Numeric scalar of the UTM zone (see details).
#' @details
#'   For locations in the North American Great Lakes, zone 15 contains the
#'   Minnesota and Wisconsin part of Lake Superior; zone 16 has has rest of
#'   Lake Superior, all of Lake Michigan, and a small portion of western
#'   Lake Huron; zone 17 has rest of Lake Huron, all of Lake Erie, and
#'   the western half of Lake Ontario; and zone 18 has rest of Lake Ontario.
#' @return
#'   A data frame with two columns (easting and northing) and as many rows as
#'   the length of \code{lon} containing the converted UTM coordinates in
#'   meters.
#' @export
#' @references
#'   Based on a function posted by Stanislav on 13 May 2015 on stackoverflow
#'   \href{http://stackoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm}{[link]}.
#' @import
#'   sp rgdal
#' @examples
#' longitude <- c(-82.27, -83.42, -81.01)
#' latitude <- c(44.76, 45.41, 45.45)
#' longlat2utm(longitude, latitude, 17)
#'
longlat2utm <- function(lon, lat, zone) {
  xy <- data.frame(easting=lon, northing=lat)
  coordinates(xy) <- c("easting", "northing")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
  res <- spTransform(xy, CRS(paste0("+proj=utm +zone=", zone, " ellps=WGS84")))
  return(as.data.frame(res))
}
