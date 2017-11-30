#' Regularize activity data
#'
#' \code{regularize activity} regularizes the timestamps in column 'ts' to
#'   regular intervals for each animal_tag. The interval is defined by the
#'   parameter \code{minutes}. The following steps are performd:
#'   \enumerate{
#'     \item Round each timestamp to the nearsed regular timestamp.
#'     \item When several data points occur with the same regularized timestamp,
#'        aggregate these by calculation the mean of the activity values.
#'     \item Insert data points for missing regularized timestamps with
#'       NA-activity values .
#'     }
#'
#' @param activity A data.table with the activity data. The following columns
#'   should be present: 'animal_tag', 'ts', 'act_x' and 'act_y'.
#' @param minutes The regular interval in minutes.
#' @return The original data.table with regularized timestamps in column 'ts'.
#' @examples
#' activity_data <- regularize_activity(data = activity_data, minutes = 5)
#' @import data.table
#' @export


regularize_activity <- function(activity,
                                minutes) {

  activity[, ts := as.POSIXct(round(as.numeric(ts)/(minutes*60))*(minutes*60),
                              origin=(as.POSIXlt('1970-01-01')))]

  if (nrow(activity[,.N, by = .(animal_tag, ts)][N!=1,.N, by = .(N)]) >0 ){
    activity <- activity[,.(act_x = as.integer(mean(act_x, na.rm=T)),
                            act_y = as.integer(mean(act_y, na.rm=T))),
                         by = .(animal_tag,ts)]
    }

  setkey(activity, animal_tag, ts)
  activity <-
    activity[setkey(activity[, seq(min(ts),max(ts),
                                   by = paste(minutes," min", sep = "")),
                  by = animal_tag], animal_tag, V1)]
  activity <- activity[order(animal_tag, ts)]
  return(activity)
}
