#' Smooth activity data
#'
#' \code{smooth_activity_data} smoothes an activity variable by calculating a
#'   moving average with window width specified by \code{width_axis_ma}.
#'
#' @param activity
#' @param axis
#' @param width_axis_ma
#' @param update_NA
#' @return  A data.table
#' @examples
#' activity_data <- smooth_activity_data(activity = activity_data,
#'                                           axis = 'act_xy',
#'                                           width_axis_ma = 2,
#'                                           update_NA = FALSE)
#' @import data.table
#' @export


smooth_activity <- function(activity,
                            axis,
                            width_axis_ma,
                            update_NA = TRUE) {

  activity <- activity[order(animal_tag, ts),,]

  column_name <- paste(axis,"_ma", width_axis_ma, sep = "")
  # calculate moving average over act_xy
  activity[, (column_name) :=
               as.integer(zoo::rollapply(get(axis), width = width_axis_ma,
                                    FUN = function(x) round(mean(x, na.rm =T)),
                                                     partial = T, align = "center")),
                by = animal_tag]

  # when all values of the window are NA, rollapply sets NaN --> replace NaN with NA
  activity[is.na(get(column_name)), (column_name) := NA,]

if (update_NA == TRUE) {
  # update single NA-values in act_xy

  activity[is.na(get(axis)) & !is.na(get(column_name)),
                (axis) := as.integer(get(column_name)),]
  }
  return(activity)
}
