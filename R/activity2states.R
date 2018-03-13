#' Activity2states
#'
#' \code{activity2states}
#'
#' @param activity A data.table with the activity data. The following columns
#'   should be present: 'animal_tag' and 'ts'.
#' @param activity_gaps The data.table with the identified activity gaps.
#' @param axis
#' @param axis_ma
#' @param width_axis_ma
#' @param threshold
#' @param min_duration_active_state
#'
#' @return
#' @examples
#'
#'
#' @import data.table
#' @export


# 1) calculate a moving window
# 2) select those datapoints where the moving window is crossed by the threshold
#    value as starting points of active and resting states
# 3) large window sizes cause a divergence of the true starting points
#        final start of active/resting states to the first/last datapoint inside
#        the window that exceeds the threshold value
# 4) remove active states that are shorter than the window size

activity2states <- function(activity,
                            activity_gaps = NULL,
                            axis = 'act_xy',
                            axis_ma = 'no',
                            width_axis_ma,
                            threshold,
                            min_duration_active_state) {

  # check if there are enough (n > 5) activity data, otherwise stop
  if (nrow(activity) < 5) {
    # return an empty table
    return(data.table("state_id" = integer(),
                      "to_active" = as.POSIXct(character()),
                      "end_active" = as.POSIXct(character()),
                      "duration" = as.difftime(character(), units = "mins"),
                      "act_mean" = numeric(),
                      "act_var" = numeric()))

  } else {

    # order the data
    activity <- activity[order(animal_id, ts),,]

    # calculate moving average if not supplied
    if (sum(names(activity) == axis_ma) == 0 | axis_ma == 'no') {
      activity[,(axis_ma) := zoo::rollapply(get(axis),
                                            width = width_axis_ma,
                                            FUN = function(x) round(mean(x, na.rm =T)),
                                            partial = T,
                                            align = "center"),]
      }

    # some data points in axis_ma have very small negative numbers that
    # have to be changed

    activity[, (axis_ma) := sapply(get(axis_ma), function(x) max(0,x)),]

    # pad start and end points of data gaps with "axis_ma <- -1" to allow for
    # easy state recognition

  if (!is.null(activity_gaps)){
    activity_gaps <-
        activity_gaps[animal_id == activity[,unique(animal_id),],,]

    if (nrow(activity_gaps) > 0) {
      insert_activity_data_gaps <- rbind(activity_gaps[,.(animal_id,
                                                          ts = to_NA),],
                                         activity_gaps[,.(animal_id,
                                                          ts = end_NA),],
                                         fill = TRUE)
      insert_activity_data_gaps[,(axis_ma) := -1,]

      activity <- rbind(activity, insert_activity_data_gaps, fill = TRUE)
      activity <- activity[order(animal_id, ts),,]

      rm(insert_activity_data_gaps)
      }
    }

    # if data gaps (NA) were not removed before, pull them to resting states
      activity[is.na(get(axis_ma)), (axis_ma) := -1,]

    # calculate start end end timestamps of active states
    active_states <-
      activity[,.(to_active = ts[which(diff(c(-1,get(axis_ma),-1) >= threshold) == 1)],
                       end_active = ts[which(diff(c(-1,get(axis_ma),-1) >= threshold) == -1)-1],
                       to_active_num = as.numeric(which(diff(c(-1,get(axis_ma),-1) >= threshold) == 1)),
                       end_active_num = as.numeric(which(diff(c(-1,get(axis_ma),-1) >= threshold) == -1)-1)),]

    # calculate duration of active periods
    active_states[, duration := difftime(end_active, to_active, units = "mins"),]

    # remove active states that are shorter than the minimum duration of active states
    active_states <- active_states[duration >= as.difftime(min_duration_active_state, units = "mins"),,]

    # find the exact transition points

    if (nrow(active_states) == 0) {
      # return an empty table
      return(data.table("state_id" = integer(),
                        "to_active" = as.POSIXct(character()),
                        "end_active" = as.POSIXct(character()),
                        "duration" = as.difftime(character(), units = "mins"),
                        "act_mean" = numeric(),
                        "act_var" = numeric()))
    } else {

      activity_vector <- activity[,get(axis),]
      ts_vector <- activity[,ts,]

       # error1
      # short intervals that are counted as active because very few data
      # points at the outer part of the dataset cause a crossing
      # of the cutoff --> remove them

      active_states[,error1:= sum(activity_vector[to_active_num:end_active_num] >= threshold, na.rm =T) == 0,
                    by = 1:nrow(active_states)]
      active_states <- active_states[error1==FALSE,][,error1 := NULL]

      # trim active states to the actual start and end points of active states and
      # calculate the mean and variance of the activity data inside the active states

      active_states[,to_active :=
                      min(ts_vector[to_active_num:end_active_num][
                        activity_vector[to_active_num:end_active_num] >= threshold]),
                    by = 1:nrow(active_states)]
      active_states[,end_active :=
                      max(ts_vector[to_active_num:end_active_num][
                        activity_vector[to_active_num:end_active_num] >= threshold]),
                    by = 1:nrow(active_states)]
      active_states[,act_mean :=
                      round(mean(activity_vector[to_active_num:end_active_num][
                        activity_vector[to_active_num:end_active_num] >= threshold], na.rm = T)),
                    by = 1:nrow(active_states)]
      active_states[,act_var :=
                      round(var(activity_vector[to_active_num:end_active_num][
                        activity_vector[to_active_num:end_active_num] >= threshold], na.rm = T)),
                    by = 1:nrow(active_states)]


      # refresh the duration
      active_states[, duration := round(difftime(end_active, to_active, units = "mins")),]
      # remove active states that are shorter than the minimum duration of active states
      active_states <- active_states[duration >= as.difftime(min_duration_active_state, units = "mins"),,]

    if (nrow(active_states) == 0) {
        # return an empty table
        return(data.table("state_id" = integer(),
                          "to_active" = as.POSIXct(character()),
                          "end_active" = as.POSIXct(character()),
                          "duration" = as.difftime(character(), units = "mins"),
                          "act_mean" = numeric(),
                          "act_var" = numeric()))
      } else {
          return(active_states[,.("state_id" = 1:.N,
                              to_active,
                              end_active,
                              duration,
                              act_mean,
                              act_var),])
      }
      }
    }
  }

