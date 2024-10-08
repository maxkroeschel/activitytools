#' Regularize activity data
#'
#' \code{regularize_activity} regularizes the timestamps within the
#'   \code{$acticity_data} of an \code{activity} object (column 'ts') to
#'   regular intervals for each animal_tag. The interval is defined by the
#'   parameter \code{minutes}. The following steps are performed:
#'
#'   \enumerate{
#'     \item Round each timestamp to the nearsed regular timestamp.
#'     \item When several data points occur with the same regularized timestamp,
#'        aggregate these by calculating the mean of the activity values.
#'     \item Insert data points for missing regularized timestamps with
#'       NA-activity values .
#'     }
#'
#' @param activity An object of class \code{activity}.
#' @param act.reg_minutes The regular interval in minutes.
#' @return The original activity object with regularized timestamps for its
#'   \code{$acticity_data} in column 'ts'.
#' @examples
#' activity_data <- regularize_activity(data = activity_data, minutes = 5)
#' @import data.table
#' @export


regularize_activity <- function(activity,
                                act.reg_minutes = NULL) {
  # Type check
  if(!is(activity, "activity")){
    stop("Please provide an object of class 'activity'")
  }

  # Change in activity object those parameters that have been provided through
  # the function call
  pars <- as.list(match.call())
  activity <- set_parameters(x = activity, parameters = pars)

  # Extract parameters from activity object
  parameters <- get_parameters(x = activity, parameters = c("act.reg_minutes",
                                                            "act.available_act"))
  minutes <- parameters$act.reg_minutes
  available_act <- parameters$act.available_act
  # Get activity data from activity object
  activity_data <- activity$activity_data

  activity_data[, ts := as.POSIXct(round(as.numeric(ts)/(minutes*60))*(minutes*60),
                                   origin=(as.POSIXlt('1970-01-01')))]

  calculate_mean <- function(x) {as.integer(mean(x, na.rm =T))}

  if (nrow(activity_data[,.N, by = .(animal_tag, ts)][N!=1,.N, by = .(N)]) >0 ){
    activity_data <- activity_data[, lapply(.SD, calculate_mean),
                                   .SDcols = available_act,
                                   by = .(animal_tag,ts) ]
  }

  setkey(activity_data, animal_tag, ts)
  activity_data <-
    activity_data[setkey(activity_data[, seq(min(ts),max(ts),
                                             by = paste(minutes," min", sep = "")),
                                       by = animal_tag], animal_tag, V1)]
  activity_data <- split_animaltag(activity_data)

  attr(activity_data$ts, "tzone") <- 'UTC'

  col_req <-  c("animal_tag", "animal_id", "tag_code", "ts", available_act)
  col_oth <- names(activity_data)[!names(activity_data) %in% col_req]
  setcolorder(activity_data, c(col_req, col_oth))
  activity$activity_data <- activity_data[order(animal_tag, ts)]

  return(activity)
}
