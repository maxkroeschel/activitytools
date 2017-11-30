#' Remove activity gaps
#'
#' \code{remove_activity_gaps} removes data gaps in the activity data. The data
#'   gaps have to be identified before with the function
#'   \code{\link{identify_activity_gaps}}.
#'
#' @param activity A data.table with the activity data. The following columns
#'   should be present: 'animal_tag' and 'ts'.
#' @param activity_gaps The data.table with the identified activity gaps.
#' @return
#' @examples
#' activity_data <- remove_activity_gaps(activity = activity_data,
#'                                       activity_gaps = activity_data_gaps)
#' @import data.table
#' @export


remove_activity_gaps <- function(activity,
                                 activity_gaps) {
  if (nrow(activity_gaps) > 0){
    for (i in 1:nrow(activity_gaps)){
      activity <-  activity[!(animal_tag == activity_gaps[i,animal_tag] &
                              ts >= activity_gaps[i,to_NA] &
                              ts <= activity_gaps[i,end_NA]),,]
    }
  }
  activity <- activity[order(animal_tag, ts),,]
  return(activity)
}
