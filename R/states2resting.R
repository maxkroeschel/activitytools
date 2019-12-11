#' Calculate resting states
#'
#' Calculates resting states based on the calculated active states for each
#' animal.
#'
#' @param activity A data.table with the activity data.
#' @param active_states A data.table with the predicted active states (return
#'   of function \code{\link{thresholds2states}}.
#' @param activity_gaps A data.table with the identified gaps in the activity
#'    data (return of function \code{\link{identify_activity_gaps}}.
#' @param reg_minutes
#' @param act_ma
#'
#' @return A data.table with the resting states.
#' @examples
#' active_states_a <-  calculate_resting_states(activity = activity,
#'                                       activity_gaps = activity_gaps,
#'                                       thresholds = activity_thresholds_final,
#'                                       threshold_par = 'threshold_a')
#'
#' @import data.table
#' @export

states2resting <- function(activity,
                           active_states,
                           activity_gaps,
                           reg_minutes,
                           act_ma) {

return(do.call("rbind",
               lapply(active_states[,unique(animal_tag),],
                      function(d_animal_tag) {

temp_activity <- activity[animal_tag == d_animal_tag,,]
temp_active_states <- active_states[animal_tag == d_animal_tag,,]
temp_activity_gaps <- activity_gaps[animal_tag == d_animal_tag,,]


# invert to resting states

temp_resting_states <-
  temp_active_states[,.("animal_tag" = animal_tag,
                        "to_resting" = end_active + lubridate::minutes(reg_minutes),
                        "end_resting" = shift(to_active, n = 1, type = "lead")
                                         - lubridate::minutes(reg_minutes)), ]

# end_resting can't be assigned in the last row --> remove it
temp_resting_states <- na.omit(temp_resting_states)

# assign begining and end
# if activity data do not start with an active state --> add a resting state at the beginning
if (temp_activity[, min(ts),] < temp_active_states[, min(to_active),] ) {
  temp_resting_states <-
    rbind(temp_resting_states,
          data.table("animal_tag" = d_animal_tag,
                     "to_resting" =  temp_activity[, min(ts),],
                     "end_resting" = temp_active_states[, min(to_active) - lubridate::minutes(reg_minutes),]))
}
# if activity data do not end with an active state --> add a resting state at the end
if (temp_activity[, max(ts),] > temp_active_states[, max(end_active),] ) {
  temp_resting_states <-
    rbind(temp_resting_states,
          data.table("animal_tag" = d_animal_tag,
                      "to_resting" = temp_active_states[, max(end_active) + lubridate::minutes(reg_minutes),],
                      "end_resting" =  temp_activity[, max(ts),]))
}

attr(temp_resting_states$to_resting, "tzone") <- NULL
attr(temp_resting_states$end_resting, "tzone") <- NULL

attr(temp_activity_gaps$to_NA, "tzone") <- NULL
attr(temp_activity_gaps$end_NA, "tzone") <- NULL


# remove data gaps
## there are two cases
## 1 the data gap extents over the whole resting state or
## 2 the data gap is somewhere in the middle

if (nrow(temp_activity_gaps) > 0) {
  setkey(temp_resting_states, animal_tag, to_resting, end_resting)
  setkey(temp_activity_gaps, animal_tag, to_NA, end_NA)
  
  changes <- data.table::foverlaps(x = temp_activity_gaps,
                                   y = temp_resting_states,
                                   by.x = c("animal_tag", "to_NA", "end_NA"),
                                   by.y = c("animal_tag", "to_resting", "end_resting"),
                                   type = "within")
  
  changes <- changes[!is.na(to_resting),, ]
  
  for (i in (1:nrow(changes))) {
    if (changes[i, to_resting == to_NA & end_resting == end_NA]) {
      temp_resting_states <- temp_resting_states[!(animal_tag == changes[i, animal_tag,] &
                                           to_resting == changes[i, to_resting,]),,]} else
      if (changes[i, to_resting == to_NA ]) {
        temp_resting_states[animal_tag == changes[i, animal_tag,] &
                         to_resting == changes[i, to_resting,],
                       to_resting := changes[i, end_NA,] + lubridate::minutes(reg_minutes),]} else
        if (changes[i, end_resting == end_NA ]) {
          temp_resting_states[animal_tag == changes[i, animal_tag,] &
                           to_resting == changes[i, to_resting,],
                         end_resting := changes[i, to_NA,] - lubridate::minutes(reg_minutes),]}
  }
  
  rm(changes)
}
## calculate summary statistics

temp_activity[, tmp_ts := ts, ]
setkey(temp_resting_states, animal_tag, to_resting, end_resting)
setkey(temp_activity, ts, tmp_ts)

temp_resting_states <-
  data.table::foverlaps(x = temp_activity,
                        y = temp_resting_states,
                        by.x = c("animal_tag", "ts", "tmp_ts"),
                        by.y = c("animal_tag", "to_resting", "end_resting"),
                        type = "within")

temp_resting_states <-
  temp_resting_states[!is.na(to_resting),.("duration" = .N,
                                           "act_mean" = round(mean(get(act_ma))),
                                           "act_var" = round(var(get(act_ma)))),
                      by = .(animal_tag, to_resting, end_resting)]

temp_resting_states <- split_animaltag(temp_resting_states)
return(temp_resting_states[, .("state_id" = paste("r.",1:.N, sep =""),
                               to_resting,
                               end_resting,
                               duration,
                               act_mean,
                               act_var,
                               animal_id,
                               tag_code,
                               animal_tag),])
})))}
