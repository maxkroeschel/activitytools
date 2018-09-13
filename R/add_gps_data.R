#' Add GPS data
#'
#' \code{add_gps_data()} adds or changes the \code{$gps_data} of a \code{states} or
#'   \code{activity} object.
#'
#' @param x
#' @param gps_data
#' @return  An object of class \code{activity} or \code{states}
#' @examples
#'   states()
#' @import data.table
#' @export

add_gps_data <- function(x,
                         gps_data){

  if(!any(is(x, "activity"), is(x, "states"))){
    stop("Please provide an object of class 'activity' or 'states'")
  }

  gps_columns <- c("animal_id", "tag_code", "longitude", "latitude", "ts")
  check_gps <- gps_columns %in% names(gps_data)
  if(!all(check_gps)){
    stop("The GPS data are missing the following columns: ", paste(gps_columns[!check_gps], collapse = ", "))
  }
  # if timestamp is not POSIXct, convert and assume UTC
  if(!is(activity_data$ts, "POSIXct")){
    activity_data$ts <- as.POSIXct(activity_data$ts, tz="UTC")
  }

  gps_data <- data.table(gps_data)[order(animal_id, ts),,]
  gps_data <- create_animaltag(gps_data)
  col_req <- c("animal_tag", "animal_id", "tag_code",
               "longitude", "latitude", "ts")
  col_oth <- names(gps_data)[!names(gps_data) %in% col_req]
  setcolorder(gps_data, c(col_req, col_oth))

  x$gps_data <- gps_data
  return(x)
}

