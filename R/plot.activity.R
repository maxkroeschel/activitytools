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
                          ...){
  if(!select %in% c("activity", "thresholds")){
    stop("The 'select' argument must be either 'activity' or 'thresholds'.")
  }
  if(select == "activity"){
    plot_activity(x, ...)
  }
  if(select == "thresholds"){
    plot_thresholds(x, ...)
  }
}
