#' Calculate activity thresholds
#'
#' \code{calculate_thresholds} calculates and aggregates activity thresholds
#'   for an \code{$activity} object, based on its \code{$activity_data} and
#'   \code{$activity_gaps}. Raw thresholds (per day) are stored in
#'   \code{$activity_data} and aggregate thresholds are stored in
#'   \code{$activity_data}
#'
#' @param activity An object of class \code{activity}.
#' @param act
#' @param reg_minutes
#' @param thresh.n_runs
#' @param thresh.window_width_around_day
#' @param thresh.n_thresholds
#' @param thresh.resting_range_limit
#' @param thresh.threshold_range_limit
#' @param states.min_duration_active
#' @param plot_summary
#' @return  An object of class \code{activity}.
#' @examples
#'   calculate_thresholds()
#' @import data.table
#' @export

calculate_thresholds <- function(activity,
                                act.act = NULL,
                                act.reg_minutes = NULL,
                                thresh.n_runs = 1,
                                thresh.window_width_around_day = NULL,
                                thresh.n_thresholds = NULL,
                                thresh.resting_range_limit = 0.2,
                                thresh.threshold_range_limit = 0.8,
                                states.min_duration_active = NULL,
                                plot_summary = FALSE) {

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
                               parameters = c("act.act",
                                              "act.width_ma",
                                              "act.reg_minutes",
                                              "thresh.n_runs",
                                              "thresh.window_width_around_day",
                                              "thresh.n_thresholds",
                                              "thresh.resting_range_limit",
                                              "thresh.threshold_range_limit",
                                              "states.min_duration_active"))
  parameters$act_ma <- paste(parameters$act.act,"_ma", parameters$act.width_ma, sep = "")

  # Get activity data and gaps from activity object
  activity_data <- activity$activity_data
  activity_gaps <- activity$activity_gaps

  # Calculate thresholds
  activity_thresholds_raw <-
    activity2thresholds(activity = activity_data,
                        activity_gaps = activity$data_gaps,
                        act = parameters$act.act,
                        act_ma = parameters$act_ma,
                        reg_minutes = parameters$act.reg_minutes,
                        n_runs = parameters$thresh.n_runs,
                        window_width_around_day = parameters$thresh.window_width_around_day,
                        n_thresholds = parameters$thresh.n_thresholds,
                        resting_range_limit = parameters$thresh.resting_range_limit,
                        threshold_range_limit = parameters$thresh.threshold_range_limit,
                        min_duration_active_state = parameters$states.min_duration_active,
                        plot_summary = plot_summary)

  # Create period_id inside the tables activity_thresholds and activity_data
  activity_thresholds_raw[, threshold_period := ts2yearweek(day),]
  activity_data[,threshold_period := ts2yearweek(ts),]

  # aggregate activity thresholds
  activity_thresholds_aggregated <- aggregate_thresholds(thresholds = activity_thresholds_raw)

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
                                     "warning"))

  setcolorder(activity_thresholds_aggregated, c("animal_tag",
                                                "animal_id",
                                                "tag_code",
                                                 "threshold_period",
                                                 "threshold_a",
                                                 "threshold_a_se",
                                                 "threshold_b",
                                                 "threshold_b_se",
                                                 "threshold_c",
                                                 "threshold_c_se"))

  activity$activity_thresholds_raw <- activity_thresholds_raw
  activity$activity_thresholds_aggregated <- activity_thresholds_aggregated
  return(activity)
}
