#' States
#'
#' \code{states()} creates an object of class \code{states}, based on an
#'   \code{activity} object.
#'
#' @param activity
#' @param thresholds
#' @param parameters
#' @param states.pos
#' @param states.dayshift
#' @param states.dawn_degree
#' @param states.period
#' @param states.max_na
#' @param keep_input
#' @return  An object of class \code{states}.
#' @examples
#'   states()
#' @import data.table
#' @export

states <- function(activity,
                   thresholds = c("a", "b", "c"),
                   parameters = NULL,
                   states.pos = NULL,
                   states.dayshift = NULL,
                   states.dawn_degree = NULL,
                   states.period = NULL,
                   states.max_na = NULL,
                   keep_input = TRUE)
{

  # Type check
  if(!is(activity, "activity")){
    stop("Please provide an object of class 'activity'")
  }
  # Input check
  if(all(is.na(activity$activity_gaps))){
    stop("No activity gaps identified. Use 'identify_activity_gaps()' to identify data gaps first.")
  }
  if(all(is.na(activity$activity_thresholds_aggregated))){
    stop("Activity thresholds are missing. Use 'calculate_thresholds()' first.")
  }

  # Prepare obejct structure
  states <- list(parameters = NA,
                 activity_data = NA,
                 activity_gaps = NA,
                 activity_thresholds_aggregated = NA,
                 gps_data = NA,
                 threshold_a = list(active_states = NA,
                                    prop_time_active = NA,
                                    gps_active = NA),
                 threshold_b = list(active_states = NA,
                                    prop_time_active = NA,
                                    gps_active = NA),
                 threshold_c = list(active_states = NA,
                                    prop_time_active = NA,
                                    gps_active = NA))

  # Parameters
  # First get parameters from activity object, then add parameters for states
  # object
  states$parameters <- c(activity$parameters,
                         list(states.pos = NA,
                              states.dayshift = NA,
                              states.dawn_degree = NA,
                              states.period = NA,
                              states.max_na = NA))

  # Keep input data
  if(keep_input == TRUE){
    states$activity_data <- activity$activity_data
    states$activity_gaps <- activity$activity_gaps
    states$activity_thresholds_aggregated <- activity$activity_thresholds_aggregated
  }
  states$gps_data <- activity$gps_data

  if(all(is.na(states$gps_data)) & all(is.na(states$parameters$states.pos))){
    stop("You must provide either GPS data or the parameter 'pos', which specifies the location of the research area.")
  }

  # Set class to 'states'
  states <- structure(states, class = "states")


  # Change parameters provided through argument 'parameters'
  if(!is.null(parameters)){
    states <- set_parameters(x = states, parameters = parameters)
  }
  # Change / overwrite parameters provided through function call
  pars <- as.list(match.call())
  if(!is.null(pars$states.pos)){
    pars$states.pos <- eval(pars$states.pos)
  }
  states <- set_parameters(x = states, parameters = pars)

  # Calculate activity states, proportional time active, and add activity state
  # to gps data (if it is available)

  if("a" %in% thresholds){
    print("Active states based on 'threshold a' ...")
    states$threshold_a$active_states <-
      thresholds2states(activity = activity$activity_data,
                        activity_gaps = activity$activity_gaps,
                        thresholds = activity$activity_thresholds_aggregated,
                        threshold_par = 'threshold_a')
    print("Proportional time active based on 'threshold a' ...")
    states$threshold_a$prop_time_active <-
      states2prop_time_active(active_states = states$threshold_a$active_states,
                              activity_gaps = activity$activity_gaps,
                              gps = states$gps_data,
                              pos = states$parameters$states.pos,
                              dayshift = states$parameters$states.dayshift,
                              dawn_degree = states$parameters$states.dawn_degree,
                              period = states$parameters$states.period,
                              max_na = states$parameters$states.max.na)
    if(!all(is.na(states$gps_data))){
      print("Classifying GPS data based 'threshold a' ...")
      states$threshold_a$gps_active <-
        states2gps(gps = states$gps_data,
                   activity_gaps = activity$activity_gaps,
                   active_states = states$threshold_a$active_states)
      print("..done!")
    }
  }

  if("b" %in% thresholds){
    print("Active states based on 'threshold b' ...")
    states$threshold_b$active_states <-
      thresholds2states(activity = activity$activity_data,
                        activity_gaps = activity$activity_gaps,
                        thresholds = activity$activity_thresholds_aggregated,
                        threshold_par = 'threshold_b')
    print("Proportional time active based on 'threshold b' ...")
    states$threshold_b$prop_time_active <-
      states2prop_time_active(active_states = states$threshold_b$active_states,
                              activity_gaps = activity$activity_gaps,
                              gps = states$gps_data,
                              pos = states$parameters$states.pos,
                              dayshift = states$parameters$states.dayshift,
                              dawn_degree = states$parameters$states.dawn_degree,
                              period = states$parameters$states.period,
                              max_na = states$parameters$states.max.na)
    if(!all(is.na(states$gps_data))){
      print("Classifying GPS data based 'threshold b' ...")
      states$threshold_b$gps_active <-
        states2gps(gps = states$gps_data,
                   activity_gaps = activity$activity_gaps,
                   active_states = states$threshold_b$active_states)
      print("..done!")
    }
  }

  if("c" %in% thresholds){
    print("Active states based on 'threshold c' ...")
    states$threshold_c$active_states <-
      thresholds2states(activity = activity$activity_data,
                        activity_gaps = activity$activity_gaps,
                        thresholds = activity$activity_thresholds_aggregated,
                        threshold_par = 'threshold_c')
    print("Proportional time active based on 'threshold c' ...")
    states$threshold_c$prop_time_active <-
      states2prop_time_active(active_states = states$threshold_c$active_states,
                              activity_gaps = activity$activity_gaps,
                              gps = states$gps_data,
                              pos = states$parameters$states.pos,
                              dayshift = states$parameters$states.dayshift,
                              dawn_degree = states$parameters$states.dawn_degree,
                              period = states$parameters$states.period,
                              max_na = states$parameters$states.max.na)
    if(!all(is.na(states$gps_data))){
      print("Classifying GPS data based 'threshold c' ...")
      states$threshold_c$gps_active <-
        states2gps(gps = states$gps_data,
                   activity_gaps = activity$activity_gaps,
                   active_states = states$threshold_c$active_states)
      print("..done!")
    }
  }

  return(states)
}
