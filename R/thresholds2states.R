#' Thresholds2states
#'
#' Calculates active states based on a list of threshold values (return
#'   of function \code{\link{activity2thresholds}} that is grouped by a
#'   defined period with function \code{\link{aggregate_thresholds}})
#'
#' @param activity A data.table with the activity data. The following columns
#'   should be present: 'animal_tag' and 'ts'.
#' @param activity_gaps The data.table with the identified activity gaps.
#' @param thresholds
#' @param threshold_parm
#'
#' @return Data.table with the ...
#' @examples
#' active_states_a <-  thresholds2states(activity = activity_data,
#'                                       activity_gaps = activity_data_gaps,
#'                                       thresholds = activity_thresholds_final,
#'                                       threshold_par = 'threshold_a')
#'
#' @import data.table
#' @export

thresholds2states <- function(parameters,
                              activity,
                              activity_gaps,
                              thresholds,
                              threshold_par) {

  return(do.call("rbind",
    lapply(thresholds[!is.na(get(threshold_par)),
                             as.character(unique(animal_tag)), ],
             function(d_animal_tag) {

   # print(paste("animal_tag: ",d_animal_tag, "  processing"))

    temp_active_states <-
      data.table(do.call("rbind",
                    lapply(thresholds[animal_tag == d_animal_tag &
                               !is.na(get(threshold_par)),
                              unique(threshold_period)],
                    function(x_period) {
                      tmp <- activity2states(activity = activity[animal_tag == d_animal_tag &
                                                                  threshold_period == x_period,,],
                                            activity_gaps = activity_gaps[animal_tag == d_animal_tag,,],
                                            act = parameters$act.act,
                                            act_ma = parameters$act_ma,
                                            min_duration_active_state = parameters$states.min_duration_active,
                                            threshold = thresholds[animal_tag == d_animal_tag &
                                                                     threshold_period == x_period, get(threshold_par),]
                                            )
                                       })))

    temp_active_states[, animal_id := unlist(strsplit(d_animal_tag, split = "_"))[1],]
    temp_active_states[, tag_code := unlist(strsplit(d_animal_tag, split = "_"))[2],]
    temp_active_states[, animal_tag := d_animal_tag,]

# The function above calculates the active states for each period and afterwards
#  merges these. An active state that expands over two period will be split into
#  two active states and has to be merged afterwards.

    activity_freq <-
      as.integer(names(activity[animal_tag == d_animal_tag,
                                sort(table(diff(ts)),decreasing = TRUE)[1],]))

    temp_active_states[(shift(to_active,n =1, type = "lead") - end_active) == activity_freq,
                       mark := 1]

    temp_active_states[which(mark==1) + 1,
                       to_active := temp_active_states[mark == 1,to_active,],]

    temp_active_states <- temp_active_states[is.na(mark),,][,mark := NULL]
    return(temp_active_states)
    }
    )
  ))
}



