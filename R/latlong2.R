#' Convert Latitude and Longitude to US State or County
#'
#' Convert latitude and longitude coordinates to US states or US states
#' and counties.
#' @param pointsDF
#'   A data frame in which the first two columns contain the longitude and
#'   latitude in degrees.
#' @param to
#'   A character scalar identifying the region to be identified, either "state",
#'   the default, or "county".
#' @return
#'   If \code{to="state"}, a character vector of state names, all lower case,
#'   the same length as the number of rows in \code{pointsDF}.
#'   If \code{to="county"}, a character matrix of state and county names, all
#'   lower case, with two columns and the same number of rows
#'   as \code{pointsDF}.
#' @export
#' @import
#'   sp maptools
#' @references
#'   Based on a method posted by Josh O'Brien on 6 Jan 2012 on stackoverflow
#'   \href{http://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r/8751965#8751965}{[link]}.
#' @examples
#' testPoints <- data.frame(x=c(-90, -120), y=c(44, 44), z=c("a", "b"))
#' latlong2(testPoints, to="state")
#' latlong2(testPoints, to="county")

latlong2 <- function(pointsDF, to=c("state", "county")[1]) {
  if (!(to %in% c("state", "county")))
    stop('to must be either "state" or "county"')
  if (!is.numeric(pointsDF[, 1]) | !is.numeric(pointsDF[, 2]))
    stop("The first two columns of pointsDF must be numeric")
  # Convert pointsDF to a SpatialPoints object
  pointsSP <- SpatialPoints(pointsDF[, 1:2],
    proj4string=CRS("+proj=longlat +datum=WGS84"))
  if (to == "state") {
    # Prepare SpatialPolygons object with one SpatialPolygon
    # per state (plus DC, minus HI & AK)
    IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
    states_sp <- map2SpatialPolygons(states, IDs=IDs,
      proj4string=CRS("+proj=longlat +datum=WGS84"))
    # Use 'over' to get _indices_ of the Polygons object containing each point
    indices <- over(pointsSP, states_sp)
    # Return the state names of the Polygons object containing each point
    stateNames <- sapply(states_sp@polygons, function(x) x@ID)
    return(stateNames[indices])
  } else {
    # Prepare SpatialPolygons object with one SpatialPolygon
    # per county (plus DC, minus HI & AK)
    IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
    counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
      proj4string=CRS("+proj=longlat +datum=WGS84"))
    # Use 'over' to get _indices_ of the Polygons object containing each point
    indices <- over(pointsSP, counties_sp)
    # Return the county names of the Polygons object containing each point
    countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
    statecounty <- countyNames[indices]
    do.call(rbind, strsplit(statecounty, ","))
  }
}
