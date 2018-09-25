#' Activity
#'
#' \code{activity()} creates an object of class \code{activity}
#'
#' @param activity
#' @param gps
#' @param parameters
#' @param keep_source
#' @return  An object of class \code{activity}.
#' @examples
#'   activity()
#' @import data.table
#' @export

activity <- function(activity_data,
                     gps_data = NULL,
                     parameters = NULL,
                     keep_source = TRUE){

  # Input checks for activity data
  available_activity_axes <- names(activity_data)[grepl(pattern =  'act',
                                                        names(activity_data))]

  # search for columns with activity data
  if (length(available_activity_axes) == 0){
    stop("You have to provide at least one column with activity data. The column name has to start with \"act\"!")
  }
  # check if necessary columns are present
  necessary_act_columns <- c("animal_id", "tag_code", "ts")
  check_col <- necessary_act_columns %in% names(activity_data)
  if (!all(check_col)){
    stop("The activity dataset is missing the following columns: ",
         paste(necessary_act_columns[!check_col], collapse = ", "))
  }
  # if timestamp is not POSIXct, convert and assume UTC
  if(!is(activity_data$ts, "POSIXct")){
    activity_data$ts <- as.POSIXct(activity_data$ts, tz="UTC")
  }

  # Input checks for gps data
  if(!is.null(gps_data)){
    necessary_gps_columns <- c("animal_id", "tag_code", "longitude", "latitude", "ts")
    check_gps <- necessary_act_columns %in% names(gps_data)
    if(!all(check_gps)){
      stop("The GPS data are missing the following columns: ", paste(necessary_act_columns[!check_gps], collapse = ", "))
    }
    # if timestamp is not POSIXct, convert and assume UTC
    if(!is(activity_data$ts, "POSIXct")){
      activity_data$ts <- as.POSIXct(activity_data$ts, tz="UTC")
    }
  }

  # Prepare object structure
  activity <- list(parameters = NA,
                   source_data = NA,
                   activity_data = NA,
                   activity_gaps = NA,
                   activity_thresholds_raw = NA,
                   activity_thresholds_aggregated = NA,
                   states_a = list(active_states = NA,
                                   prop_time_active = NA,
                                   gps_active = NA),
                   states_b = list(active_states = NA,
                                   prop_time_active = NA,
                                   gps_active = NA),
                   states_c = list(active_states = NA,
                                   prop_time_active = NA,
                                   gps_active = NA),
                   gps_data = NA)

  activity$parameters <- list(act.available_axes = NA,
                              act.axis = NA,
                              act.reg_minutes = NA,
                              act.smooth_width_ma = NA,
                              thresh.n_runs = NA,
                              thresh.window_width_around_day = NA,
                              thresh.n_thresholds = NA,
                              thresh.min_bin_width = NA,
                              states.min_duration_active = NA,
                              pta.pos = NA,
                              pta.dayshift = NA,
                              pta.dawn_degree = NA,
                              pta.period = NA,
                              pta.max_na = NA)

  activity$parameters$act.available_axes <- available_activity_axes
  # set class to "activity"
  activity <- structure(activity, class = "activity")

  # Add provided parameters
  if(!is.null(parameters)){
    activity <- set_parameters(x = activity, parameters = parameters)
  }

  # Add activity data
  if(keep_source == TRUE){
    activity$source_data <- data.table(activity_data)[order(animal_id, ts),,]
  }
  activity_data <- data.table(activity_data)
  activity_data <- create_animaltag(activity_data)

  col_req <-  c("animal_tag",
                "animal_id",
                "tag_code",
                "ts",
                available_activity_axes)
  col_oth <- names(activity_data)[!names(activity_data) %in% col_req]
  setcolorder(activity_data, c(col_req, col_oth))

  activity$activity_data <- activity_data
  if(!is.null(gps_data)){
    gps_data <- data.table(gps_data)[order(animal_id, ts),,]
    gps_data <- create_animaltag(gps_data)
    col_req <- c("animal_tag", "animal_id", "tag_code",
                 "longitude", "latitude", "ts")
    col_oth <- names(gps_data)[!names(gps_data) %in% col_req]
    setcolorder(gps_data, c(col_req, col_oth))
    activity$gps_data <- gps_data
  }

  return(activity)
}
