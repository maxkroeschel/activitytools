#' Smooth activity data
#'
#' \code{smooth_activity} smoothes an activity variable within the
#'   \code{$activity_data} that is part of an \code{activity} object by
#'   calculating a moving average with window width specified by
#'   \code{width_act_ma}.
#'
#' @param activity An object of class \code{activity}.
#' @param act.act
#' @param act.smooth_width_ma
#' @param update_NA
#' @return  An object of class \code{activity}.
#' @examples
#' activity_data <- smooth_activity_data(activity = activity_data,
#'                                           act = 'act_xy',
#'                                           width_act_ma = 2,
#'                                           update_NA = FALSE)
#' @import data.table
#' @export


smooth_activity <- function(activity,
                            act.act = NULL,
                            act.smooth_width_ma = NULL,
                            update_NA = TRUE) {

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
                               parameters = c("act.act", "act.smooth_width_ma"))
  act <- parameters$act.act
  width_act_ma <- parameters$act.smooth_width_ma

  # Get activity data from activity object
  activity_data <- activity$activity_data

  activity_data <- activity_data[order(animal_tag, ts),,]

  column_name <- paste(act,"_ma", width_act_ma, sep = "")
  # calculate moving average over act_xy
  activity_data[, (column_name) :=
               as.integer(zoo::rollapply(get(act), width = width_act_ma,
                                    FUN = function(x) round(mean(x, na.rm =T)),
                                                     partial = T, align = "center")),
                by = animal_tag]

  # when all values of the window are NA, rollapply sets NaN --> replace NaN with NA
  activity_data[is.na(get(column_name)), (column_name) := NA,]

  if (update_NA == TRUE) {
    # update single NA-values in act_xy

    activity_data[is.na(get(act)) & !is.na(get(column_name)),
                  (act) := as.integer(get(column_name)),]
  }
  activity$activity_data <- activity_data
  return(activity)
}
