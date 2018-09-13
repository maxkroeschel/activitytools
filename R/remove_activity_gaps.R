#' Remove activity gaps
#'
#' \code{remove_activity_gaps} removes data gaps in the \code{$activity_data}
#'   of an \code{activity} object. The data gaps have to be identified before
#'   with the function \code{\link{identify_activity_gaps}}.
#'
#' @param activity An object of class \code{activity}.
#' @return
#' @examples
#' activity_data <- remove_activity_gaps(activity = activity_data,
#'                                       activity_gaps = activity_data_gaps)
#' @import data.table
#' @export


remove_activity_gaps <- function(activity,
                                 activity_gaps) {

  # Type check
  if(!is(activity, "activity")){
    stop("Please provide an object of class 'activity'")
  }
  # Input check
  if(all(is.na(activity$activity_gaps))){
    stop("No activity gaps identified. Use 'identify_activity_gaps()' to identify data gaps first.")
  }

  # Get activity data and gaps from activity object
  activity_data <- activity$activity_data
  activity_gaps <- activity$activity_gaps

  if (nrow(activity_gaps) > 0){
    for (i in 1:nrow(activity_gaps)){
      activity_data <-  activity_data[!(animal_tag == activity_gaps[i,animal_tag] &
                              ts >= activity_gaps[i,to_NA] &
                              ts <= activity_gaps[i,end_NA]),,]
    }
  }
  activity$activity_data <- activity_data[order(animal_tag, ts),,]
  return(activity)
}
