#' Add state information to GPS positions
#'
#' \code{states2gps} classifies the GPS positions based on the predicted active
#'   states.
#'
#' @param active_states A data.table with the active states (return of function
#'  \code{\link{thresholds2states}}).
#' @param gps A data.table with the GPS-positions of the animals.
#'
#' @examples
#'
#' @return The data.table with the gps positions with additional columns
#'   <active> = specifies if the animal was active (1) or resting (0),
#'   <state_id> = ID,
#'   <active_duration> = duration,
#'   <active_act_mean> = mean and
#'   <active_act_var> = variance of the active state.
#'
#' @import data.table
#' @export

states2gps <- function(active_states,
                       gps) {
  gps[, temp_ts := ts, ]
  setkey(gps, animal_tag, ts, temp_ts)
  setkey(active_states, animal_tag, to_active, end_active)

  gps <- data.table::foverlaps(x = gps,
                                    y = active_states,
                                    by.x = c("animal_tag", "ts", "temp_ts"),
                                    by.y = c("animal_tag", "to_active", "end_active"),
                                    type = "within")

  colnames(gps)[colnames(gps) == "state_id"] <- "i.state_id"
  gps[, active := 0,][
            !is.na(to_active), active := 1, ][
              , state_id := i.state_id, ][
              , active_duration := duration, ][
               , active_act_mean := act_mean, ][
                 , active_act_var := act_var,]

  gps[, ':=' (temp_ts = NULL,
              to_active = NULL,
              end_active = NULL,
              i.state_id = NULL,
              i.animal_id = NULL,
              i.tag_code = NULL,
              duration = NULL,
              act_mean = NULL,
              act_var = NULL), ]
  return(gps)
}
