#' Identify data gaps in the activity data
#'
#' \code{identify_activity_gaps} searches for data gaps in the activity data.
#'
#' @param activity A data.table with the activity data. The following columns
#'   should be present: 'animal_tag', 'ts' and the activity variable that is
#'   scanned for gaps (e.g. 'act_x').
#' @param axis The activity variable to scan for gaps.
#' @return  A data.table with the start- and end-timestamps of the identified
#'   gaps.
#' @examples
#' activity_data_gaps <- identify_activity_gaps(activity = activity_data,
#'                                              axis = 'act_xy')
#' @import data.table
#' @export

identify_activity_gaps <- function(activity,
                                   axis) {

  activity_gaps <-
    activity[,.(to_NA = ts[which(diff(is.na(c(1,get(axis),1))) == 1)],
                     end_NA = ts[which(diff(is.na(c(1,get(axis),1))) == -1)-1]),
                  by = animal_tag]
  activity_gaps[,animal_id := as.integer(unlist(strsplit(animal_tag, split = "_"))[1]),
                     by = 1:length(animal_tag)]
  activity_gaps[,tag_code := unlist(strsplit(animal_tag, split = "_"))[2],
                     by = 1:length(animal_tag)]
  return(activity_gaps)
}
