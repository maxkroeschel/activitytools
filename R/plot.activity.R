#' Plotting method for \code{activity} objects
#'
#' \code{plot.activity} creates plots for activity object.
#'
#' @param x An object of class \code{activity}. Depending on choice of plot, it
#'   should containing \code{$ctivity_data} or aggregated activity thresholds
#'   (\code{$activity_thresholds_aggregated}), and optionally \code{gps_data}.
#' @param select Type of plot, either 'activity' or 'thresholds'.
#' @param animal_id
#' @param tag_code
#' @return
#' @examples
#' @import data.table

plot.activity <- function(x,
                          select = "activity",
                          threshold = NULL,
                          select_animal_id = NULL,
                          ...){
  # Type check
  if(!is(x, "activity")){
    stop("Please provide an object of class 'activity' or 'states'")
  }

  # Argument check
  if(!select %in% c("activity", "activity_eval", "thresholds", 'states')){
    stop("The 'select' argument must be either 'activity', 'activity_eval', 'thresholds', or 'states'.")
  }

  # Select plot type

  if(select == "activity"){
    # Input check

    if (is.null(select_animal_id)){
      select_animal_id <- as.integer(readline("Please select an animal_id: "))
    }

    if(all(is.na(x$activity_data[animal_id == select_animal_id,,]))){
      stop("No activity data found for this animal.")
    }

    # Pass to plotting function
    plot_activity(activity = x$activity_data[animal_id == select_animal_id,,],
                  thresholds = if(all(is.na(x$activity_thresholds_aggregated))) {
                                NA} else {
                                x$activity_thresholds_aggregated[animal_id == select_animal_id,,]},
                  gps = if(all(is.na(x$gps_data))) {
                          NA} else {
                          x$gps_data[animal_id == select_animal_id,,]},
                  act.available_act = x$parameters$act.available_act,
                  states_a = if(all(is.na(x$states_a))) {
                              NA} else {
                              x$states_a$active_states[animal_id == select_animal_id,,]},
                  states_b = if(all(is.na(x$states_b))) {
                              NA} else {
                              x$states_b$active_states[animal_id == select_animal_id,,]},
                  states_c = if(all(is.na(x$states_c))) {
                              NA} else {
                              x$states_c$active_states[animal_id == select_animal_id,,]},
                  select_animal_id = select_animal_id,
                  ...)
  }

  if(select == "activity_eval"){
    # Input check
    if(all(is.na(x$activity_data[animal_id == select_animal_id,,]))){
      stop("No activity data found for this animal.")
    }

    # Pass to plotting function
    plot_activity_eval(activity = x$activity_data[animal_id == select_animal_id,,],
                       gps = if(all(is.na(x$gps_data))) {
                         NA} else {
                           x$gps_data[animal_id == select_animal_id,,]},
                       act.available_act = x$parameters$act.available_act,
                       select_animal_id = select_animal_id,
                       ...)
  }


  if(select == "thresholds"){
    # Input check
    if(all(is.na(x$activity_thresholds_aggregated))){
      stop("No activity thresholds to plot.")
    }
    # Pass to plotting function
    plot_thresholds(thresholds = x$activity_thresholds_aggregated,
                    ...)
  }

  if(select == "states"){
    # Threshold identifier
    th <- paste0("states_", threshold)
    # Input check
    if(length(threshold) != 1){
      stop("Please provide exactly one threshold.")
    }
    if(!threshold %in% c("a", "b", "c")){
      stop("Threshold must be either 'a', 'b' or 'c'")
    }
    if(all(is.na(x[[which(names(x) == th)]]$active_states))){
      stop("No active states found for this threshold.")
    }
    if(all(is.na(x[[which(names(x) == th)]]$prop_time_active))){
      stop("Proportional time active is missing for this threshold.")
    }
    # Pass to plotting function
    plot_states(active_states = x[[which(names(x) == th)]]$active_states,
                prop_time_active = x[[which(names(x) == th)]]$prop_time_active,
                gps = x$gps_data,
                ...)
  }
}
