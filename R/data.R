#' Activity data.
#'
#' A data set containing the activity data of three tagged red deer. The sensor
#'  measured acceleration in two axis (x = forward–backward horizontal motion
#'  and y =  left–right horizontal motion) and was set to record activity
#'  every 5 minutes.
#'
#' @format A data frame with 5 variables and 301262 rows:
#' \describe{
#'   \item{animal_id}{Identifier of the animal.}
#'   \item{tag_code}{Identifier of the tag.}
#'   \item{act_x}{Activity values of the x-axis.}
#'   \item{act_y}{Activity values of the y-axis.}
#'   \item{ts}{Timestamp of the activity measurement at time zone UTC.}
#' }
#' @source Forest Research Institute of Baden-Wuerttemberg, Division of Wildlife
#'         Ecology}
"activity_data"

#' GPS data.
#'
#' A data set containing the GPS data of three tagged red deer. The sensor was
#' set to acquire a GPS position every two hours.
#'
#' @format A data frame with 6 variables and 15780 rows:
#' \describe{
#'   \item{animal_id}{Identifier of the animal.}
#'   \item{tag_code}{Identifier of the tag.}
#'   \item{longitude}{Longitude coordinate of the GPS position in WGS84
#'         (EPSG:4326).}
#'   \item{latitude}{Latitude coordinate of the GPS position in WGS8
#'         (EPSG: 4326).}
#'   \item{dop}{Dilution of precision of the GPS position.}
#'   \item{ts}{Timestamp of the GPS position.}
#' }
#' @source Forest Research Institute of Baden-Wuerttemberg, Division of Wildlife Ecology}
"gps_data"
