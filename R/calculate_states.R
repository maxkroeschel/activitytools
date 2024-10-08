#' Calculate activity states
#'
#' \code{calculate_states()} calculates active states for an \code{activity} object.
#'
#' @param activity
#' @param thresholds
#' @param add
#' @param pta.pos
#' @param pta.dayshift
#' @param pta.dawn_degree
#' @param pta.period
#' @param pta.max_na
#' @param keep_input
#' @return  An object of class \code{activity}.
#' @examples
#'   calculate_states()
#' @import data.table
#' @export

calculate_states <- function(activity,
                             thresholds = c("a", "b", "c"),
                             add = NULL,
                             pta.pos = NULL,
                             pta.dayshift = NULL,
                             pta.dawn_degree = NULL,
                             pta.period = NULL,
                             pta.max_na = NULL)
{

  # Type check
  if(!is(activity, "activity")){
    stop("Please provide an object of class 'activity'")
  }
  # Input check
  if(length(activity$activity_gaps) == 1){
    stop("No activity gaps identified. Use 'identify_activity_gaps()' to identify data gaps first.")
  }
  if(all(is.na(activity$activity_thresholds_aggregated))){
    stop("Activity thresholds are missing. Use 'calculate_thresholds()' first.")
  }

  # Parameters
  # Change / overwrite parameters provided through function call
  pars <- as.list(match.call())
  if(!is.null(pars$pta.pos)){
    pars$pta.pos <- eval(pars$pta.pos)
  }
  activity <- set_parameters(x = activity, parameters = pars)

  # Extract parameters from activity object
  parameters <- get_parameters(x = activity,
                               parameters = c("act.act",
                                              "act.width_ma",
                                              "act.reg_minutes",
                                              "thresh.n_runs",
                                              "thresh.window_width_around_day",
                                              "thresh.n_thresholds",
                                              "states.min_duration_active"))
  parameters$act_ma <- paste(parameters$act.act,"_ma", parameters$act.width_ma, sep = "")

  if(!any(c("a", "b", "c") %in% thresholds)){
    stop("Invalid threshold(s). Accepted threshold types are 'a', 'b', or 'c'")
  }

  if(!is.null(add) & !any(c("pta", "resting", "gps") %in% add)){
    stop("Invalid option(s). Accepted option types are 'pta', 'resting' and 'gps'.")
  }

  if(("gps" %in% add) & all(is.na(activity$gps_data)) & all(is.na(activity$parameters$pta.pos))){
    stop("You must provide either GPS data or the parameter 'pos', which specifies the location of the research area.")
  }

  # Calculate activity states, proportional time active, and add activity state
  # to gps data (if it is available)

  for(t in thresholds){
    if(t %in% c("a", "b", "c")){
      print(paste0("Calculate active states based on 'threshold ", t, "' ..."))
      states_t <- paste0("states_", t)
      activity[[states_t]]$active_states <-
        thresholds2states(parameters = parameters,
                          activity = activity$activity_data,
                          activity_gaps = activity$activity_gaps,
                          thresholds = activity$activity_thresholds_aggregated,
                          threshold_par = paste('threshold_', t, sep = ""))
      print("..done!")

    if("pta" %in% add){
      print(paste0("Calculate proportional time active based on 'threshold ", t, "' ..."))
      activity[[states_t]]$prop_time_active <-
        states2prop_time_active(active_states = activity[[states_t]]$active_states,
                                activity_gaps = activity$activity_gaps,
                                gps = activity$gps_data,
                                pos = activity$parameters$pta.pos,
                                dayshift = activity$parameters$pta.dayshift,
                                dawn_degree = activity$parameters$pta.dawn_degree,
                                period = activity$parameters$pta.period,
                                max_na = activity$parameters$pta.max.na)
      print("done!")
    }
    if("resting" %in% add){
        print(paste0("Calculate resting states based on 'threshold ", t, "' ..."))
        activity[[states_t]]$resting_states <-
          states2resting(activity = activity$activity_data,
                         active_states = activity[[states_t]]$active_states,
                         activity_gaps = activity$activity_gaps,
                         act_ma = parameters$act_ma,
                         reg_minutes = parameters$act.reg_minutes)
        print("done!")
    }

    if("gps" %in% add){
      if(!all(is.na(activity$gps_data))){

        # check if resting states are already calculated; if not --> calculate
        if (is.null(nrow(activity[[states_t]]$resting_states))){
          print(paste0("Resting States based on 'threshold ", t, "' ..."))
          activity[[states_t]]$resting_states <-
            states2resting(activity = activity$activity_data,
                           active_states = activity[[states_t]]$active_states,
                           activity_gaps = activity$activity_gaps,
                           act_ma = parameters$act_ma,
                           reg_minutes = parameters$act.reg_minutes)
          print("done!")
        }

        print(paste0("Classifying GPS data based on 'threshold ", t, "' ..."))
        activity[[states_t]]$gps_data <-
          states2gps(gps = activity$gps_data,
                     activity_gaps = activity$activity_gaps,
                     reg_minutes = activity$parameters$act.reg_minutes,
                     active_states = activity[[states_t]]$active_states,
                     resting_states = activity[[states_t]]$resting_states)
        print("done!")
      }
    }

    if("gps_tracks" %in% add){
        print(paste0("Calculate gps_tracks based on 'threshold ", t, "' ..."))
        activity[[states_t]]$gps_tracks <-
          states2tracks( active_states = activity[[states_t]]$active_states,
                         activity_gaps = activity$activity_gaps,
                         gps = activity$gps,
                         dayshift = activity$parameters$pta.dayshift,
                         dawn_degree = activity$parameters$pta.dawn_degree)
        print("done!")
      }
    }
  }
  return(activity)
}
