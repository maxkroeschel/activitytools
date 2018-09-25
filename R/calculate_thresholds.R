#' Calculate activity thresholds
#'
#' \code{calculate_thresholds} calculates and aggregates activity thresholds
#'   for an \code{$activity} object, based on its \code{$activity_data} and
#'   \code{$activity_gaps}. Raw thresholds (per day) are stored in
#'   \code{$activity_data} and aggregate thresholds are stored in
#'   \code{$activity_data}
#'
#' @param activity An object of class \code{activity}.
#' @param axis
#' @param thresh.n_runs
#' @param thresh.window_width_around_day
#' @param thresh.n_thresholds
#' @param thresh.min_bin_width
#' @param states.min_duration_active
#' @param plot_summary
#' @return  An object of class \code{activity}.
#' @examples
#'   calculate_thresholds()
#' @import data.table
#' @export

calculate_thresholds <- function(activity,
                                axis = NULL,
                                thresh.n_runs = NULL,
                                thresh.window_width_around_day = NULL,
                                thresh.n_thresholds = NULL,
                                thresh.min_bin_width = NULL,
                                states.min_duration_active = NULL,
                                plot_summary) {

  # Type check
  if(!is(activity, "activity")){
    stop("Please provide an object of class 'activity'")
  }
  # Input check
  if(length(activity$activity_gaps) == 1){
    stop("No activity gaps identified. Use 'identify_activity_gaps()' to identify data gaps first.")
  }

  # Change in activity object those parameters that have been provided through
  # the function call
  pars <- as.list(match.call())
  # allow thresh.n_thresholds to be a vector (or any other expression)
  if(!is.null(pars$thresh.n_thresholds)){
    pars$thresh.n_thresholds <- eval(pars$thresh.n_thresholds)
  }
  activity <- set_parameters(x = activity, parameters = pars)

  # Extract parameters from activity object
  parameters <- get_parameters(x = activity,
                               parameters = c("act.axis",
                                              "act.smooth_width_ma",
                                              "thresh.n_runs",
                                              "thresh.window_width_around_day",
                                              "thresh.n_thresholds",
                                              "thresh.min_bin_width",
                                              "states.min_duration_active"))
  parameters$axis_ma <- paste(parameters$act.axis,"_ma", parameters$act.smooth_width_ma, sep = "")

  # Get activity data and gaps from activity object
  activity_data <- activity$activity_data
  activity_gaps <- activity$activity_gaps

  # Calculate thresholds
  activity_thresholds_raw <-
    activity2thresholds(activity = activity_data,
                        activity_gaps = activity$data_gaps,
                        axis = parameters$act.axis,
                        axis_ma = parameters$axis_ma,
                        n_runs = parameters$thresh.n_runs,
                        window_width_around_day = parameters$thresh.window_width_around_day,
                        n_thresholds = parameters$thresh.n_thresholds,
                                   min_bin_width = parameters$thresh.min_bin_width,
                                   min_duration_active_state = parameters$states.min_duration_active,
                                   plot_summary = plot_summary)

  # Create period_id inside the tables activity_thresholds and activity_data
  activity_thresholds_raw[, threshold_period := ts2yearweek(day),]
  activity_data[,threshold_period := ts2yearweek(ts),]



  # aggregate activity thresholds
  activity_thresholds_aggregated <- aggregate_thresholds(thresholds = activity_thresholds_raw)
  # add parameter options for other aggregation intervals?

  activity_thresholds_raw <- split_animaltag(activity_thresholds_raw)
  activity_thresholds_aggregated <- split_animaltag(activity_thresholds_aggregated)

  setcolorder(activity_thresholds_raw, c("animal_tag",
                                     "animal_id",
                                     "tag_code",
                                     "day",
                                     "threshold_period",
                                     "threshold_a",
                                     "threshold_b",
                                     "threshold_c",
                                     "n_thresholds",
                                     "bin_width",
                                     "run",
                                     "warning",
                                     "axis",
                                     "axis_ma",
                                     "min_duration_active_state",
                                     "window_width_around_day"))
  setcolorder(activity_thresholds_aggregated, c("animal_tag",
                                                 "animal_id",
                                                 "tag_code",
                                                 "threshold_period",
                                                 "threshold_a",
                                                 "threshold_a_se",
                                                 "threshold_b",
                                                 "threshold_b_se",
                                                 "threshold_c",
                                                 "threshold_c_se",
                                                 "axis",
                                                 "axis_ma",
                                                 "min_duration_active_state",
                                                 "window_width_around_day"))

  activity$activity_thresholds_raw <- activity_thresholds_raw
  activity$activity_thresholds_aggregated <- activity_thresholds_aggregated
  return(activity)
}
