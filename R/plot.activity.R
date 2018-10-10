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
                          ...){
  # Type check
  if(!is(x, "activity")){
    stop("Please provide an object of class 'activity' or 'states'")
  }

  # Argument check
  if(!select %in% c("activity", "thresholds", 'states')){
    stop("The 'select' argument must be either 'activity', 'thresholds', or 'states'.")
  }

  # Select plot type

  if(select == "activity"){
    # Input check
    if(all(is.na(x$activity_data))){
      stop("No activity data found.")
    }
    if(all(is.na(x$activity_thresholds_aggregated))){
      stop("No activity thresholds found.")
    }
    y <- list("activity_data" = x$activity_data,
              "activity_thresholds_aggregated" = x$activity_thresholds_aggregated,
              "gps_data" = x$gps_data)

    # Pass to plotting function
    plot_activity(activity = y$activity_data,
                  thresholds = y$activity_thresholds_aggregated,
                  gps = y$gps_data,
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
