#' Add state information to GPS positions
#'
#' \code{states2gps} classifies GPS positions into active and resting states.
#'
#' @param active_states A data.table with the active states (return of function
#'  \code{\link{thresholds2states}}).
#' @param resting_states A data.table with the resting states (return of function
#'  \code{\link{states2resting}}).
#' @param activity_gaps A data.table with the activity gaps.
#' @param gps A data.table with the GPS-positions of the animals.
#' @param reg_minutes
#'
#' @examples
#'
#' @return The data.table with the gps positions and the following columns
#'   <active> = specifies if the animal was active ('a') or resting ('r'),
#'   <state_id> = ID of the state,
#'   <active_duration> = duration od the state,
#'   <active_act_mean> = mean of the activity values of the state and
#'   <active_act_var> = variance of the activity values of the state.
#'
#' @import data.table
#' @export

states2gps <- function(active_states,
                       resting_states,
                       activity_gaps = NULL,
                       reg_minutes,
                       gps) {

  # active_states <- act_red$states_b$active_states
  # resting_states <- act_red$states_b$resting_states
  # activity_gaps <- act_red$activity_gaps
  # reg_minutes <- act_red$parameters$act.reg_minutes
  # gps <- act_red$gps_data

  col_gps <- colnames(gps)
  gps <- copy(gps)

# merge active and resting states
  active_states[, state := "a",]
  resting_states[, state := "r",]
  names(active_states)[names(active_states)== "to_active"] <- "state_start_ts"
  names(active_states)[names(active_states)== "end_active"] <- "state_end_ts"
  names(resting_states)[names(resting_states)== "to_resting"] <- "state_start_ts"
  names(resting_states)[names(resting_states)== "end_resting"] <- "state_end_ts"

  states <- rbind(active_states, resting_states)

  # add
  states[, state_end_ts := state_end_ts +
                          lubridate::minutes(reg_minutes) - lubridate::seconds(1),]

  #attr(states$state_end_ts, "tzone") <- NULL
  #attr(states$state_start_ts, "tzone") <- NULL

  gps[, temp_ts := ts, ]
  setkey(gps, animal_tag, ts, temp_ts)
  setkey(states, animal_tag, state_start_ts, state_end_ts)

  gps <- data.table::foverlaps(x = gps,
                               y = states,
                               by.x = c("animal_tag", "ts", "temp_ts"),
                               by.y = c("animal_tag", "state_start_ts", "state_end_ts"),
                               type = "within")
  col_new <- c("state_id", "state_start_ts", "state_end_ts",
  "duration", "act_mean", "act_var", "state")

  cols_remove <- names(gps)[!(names(gps) %in% c(col_gps, col_new))]

  for (i in cols_remove) {
    gps[ , paste(i) := NULL, ]}

  setcolorder(gps, c(col_gps, c("state_id", "state_start_ts", "state_end_ts",
                                "duration", "act_mean", "act_var")))

  return(gps)
}
