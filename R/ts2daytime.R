#' Calculate solar elevation or daytime.
#'
#' \code{ts2daytime} calculates solar elevation or daytime for a given timestamp
#'   and location on earth.
#'
#' @param long A vector with longitude coordinates in WGS84.
#' @param long A vector with latitude coordinates in WGS84.
#' @param ts A vector with timestamps in POSIXct.
#' @param type Either "daytime" or "elevation".
#' @param dawn_degree A integer defining sun elevation below the horizon at
#'   start of dawn and end of dusk (12 refers to a sun elevation of -12 degree)
#'
#' @examples
#'  ts2daytime(lon = 7.82,
#'              lat = 48.00,
#'              ts = as.POSIXct("2017-01-01 12:00:00"),
#'              type = "daytime",
#'              dawn_degree = 12)
#'
#' @return A vector with daytimes ("night", "dawn", "day", "dusk") when type =
#'   "daytime" or a vector with sun elevation in degrees when type = "elevation".
#'
#' @import data.table
#' @export

ts2daytime <- function(long,
                       lat,
                       ts,
                       dawn_degree = 12,
                       type = "daytime") {


  pos <- matrix(c(long, lat), nrow = length(long))
  angle <- round(maptools::solarpos(pos,
                                    ts), 2)
  if (type == "daytime") {
    daytime <- rep("night", times = nrow(angle))
    daytime[angle[,2] >= -dawn_degree &
              angle[,2] < 0 &
              angle[,1] <= 180] <- "dawn"
    daytime[angle[,2] >= 0] <- "day"
    daytime[angle[,2] >= -dawn_degree &
              angle[,2] < 0 &
              angle[,1] > 180] <- "dusk"
    return(daytime)
  } else if (type == "elevation") {
    return(angle[,2])
  } else {
      stop("You have to specify 'type'.")
    }
}
