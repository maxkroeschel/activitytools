#' Identify, archive and remove gaps in the activity data
#'
#' \code{remove_activity_gaps} searches for data gaps in the
#'   \code{$activity_data} of an \code{activity} object, then stores the gaps
#'   in the object as \code{$activity_gaps and removes data gaps in the \code{$activity_data}
#'   of an \code{activity} object.
#'
#' @param activity An object of class \code{activity}.
#' @param act The activity variable to scan for gaps.
#' @return  An object of class \code{activity} containing \code{$activity_gaps}
#'   in which the start- and end-timestamps of the identified gaps are
#'   specified.
#' @examples
#' activity_data_gaps <- remove_activity_gaps(activity = activity_data,
#'                                            act = 'act_xy')
#' @import data.table
#' @export

remove_activity_gaps <- function(activity,
                                 act.act = NULL) {
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
                               parameters = c("act.act"))
  act <- parameters$act.act

  # Get activity data from activity object
  activity_data <- activity$activity_data

  activity_gaps <-
    activity_data[,.(to_NA = ts[which(diff(is.na(c(1,get(act),1))) == 1)],
                     end_NA = ts[which(diff(is.na(c(1,get(act),1))) == -1)-1]),
                  by = animal_tag]
  if (nrow(activity_gaps) > 0) {
    activity_gaps[, animal_id := data.table::tstrsplit(animal_tag, split = "_")[1],]
    activity_gaps[, tag_code := data.table::tstrsplit(animal_tag, split = "_")[2],]
  } else {
    activity_gaps[, animal_id := as.character(),][, tag_code := as.character()]
  }

  activity$activity_gaps <- activity_gaps

  if (nrow(activity_gaps) > 0){
    activity_data <- activity_data[!is.na(get(act)),]
  }
  activity$activity_data <- activity_data[order(animal_tag, ts),,]

  return(activity)
}
