#' Identify gaps in the activity data
#'
#' \code{identify_activity_gaps} searches for data gaps in the
#'   \code{$activity_data} of an \code{activity} object, then stores the gaps
#'   in the object as \code{$activity_gaps}
#'
#' @param activity An object of class \code{activity}.
#' @param axis The activity variable to scan for gaps.
#' @return  An object of class \code{activity} containing \code{$activity_gaps}
#'   in which the start- and end-timestamps of the identified gaps are
#'   specified.
#' @examples
#' activity_data_gaps <- identify_activity_gaps(activity = activity_data,
#'                                              axis = 'act_xy')
#' @import data.table
#' @export

identify_activity_gaps <- function(activity,
                                   act.axis_ma = NULL) {
  # Type check
  if(!is(activity, "activity")){
    stop("Please provide an object of class 'activity'")
  }

  # Change in activity object those parameters that have been provided through
  # function call
  pars <- as.list(match.call())
  activity <- set_parameters(x = activity, parameters = pars)

  # Extract parameters from activity object
  parameters <- get_parameters(x = activity,
                               parameters = c("act.axis_ma"))
  axis <- parameters$act.axis_ma

  # Get activity data from activity object
  activity_data <- activity$activity_data

  activity_gaps <-
    activity_data[,.(to_NA = ts[which(diff(is.na(c(1,get(axis),1))) == 1)],
                     end_NA = ts[which(diff(is.na(c(1,get(axis),1))) == -1)-1]),
                  by = animal_tag]
  if (nrow(activity_gaps) > 0) {
    activity_gaps[, animal_id := as.integer(unlist(strsplit(animal_tag, split = "_"))[1]),
                   by = 1:length(animal_tag)]
    activity_gaps[, tag_code := unlist(strsplit(animal_tag, split = "_"))[2],
                   by = 1:length(animal_tag)]
  } else {
    activity_gaps[, animal_id := as.integer(),][, tag_code := as.character()]
  }

  activity$activity_gaps <- activity_gaps

  return(activity)
}
