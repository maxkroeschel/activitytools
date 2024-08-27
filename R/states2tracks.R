#' Create gps tracks and add state information
#'
#' \code{states2tracks} Create GPS tracks and and add the active minutes.
#'
#' @param active_states A data.table with the predicted active states (return
#'   of function \code{\link{thresholds2states}}.
#' @param activity_gaps A data.table with the identified gaps in the activity
#'    data (return of function \code{\link{identify_activity_gaps}}.
#' @param gps A data.table with the GPS positions (at least one) of the animals.
#'   The table should contain the columns: <animal_tag>, <longitude>, <latitude>.
#' @return A data.table with the gps tracks containing start and end timestamps
#' <ts_start, ts_end>, longitude and latitude of the start end end position
#' <long_start, lat_start, long_end, lat_end>, the distance between
#' the points <dist> and the amount of minutes in state active <active_minutes>.
#'
#' @examples states2tracks(active_states = active_states_a,
#'                                   activity_gaps = activity_data_gaps,
#'                                   gps = gps_data)

#'
#' @import data.table
#' @export

states2tracks <- function(active_states,
                          activity_gaps,
                          gps = NULL) {


  # make sure that a spatial position is available for each animal to calculate
  # position of the sun for each day
  animal_tags <- active_states[, unique(animal_tag),]

  if (!is.null(gps)) {
    hr_center <- gps[!is.na(longitude),
                     .("median_longitude" = median(as.numeric(longitude)),
                       "median_latitude" = median(as.numeric(latitude))),
                     by = animal_tag]
    missing_gps <- animal_tags[! animal_tags %in% hr_center[, unique(animal_tag), ]]
    if (length(missing_gps) > 0) {
        stop(paste('GPS positions for animal_tag: ',
                   paste(missing_gps, collapse = ", "), ' are missing. Please provide the gps data if gps tracks coordinates are to be calculatet (parameter \'gps_data\') !'))
    }} else {
      stop('GPS data are missing. Please provide the GPS data when gps tracks are to be calculated.')
      }

gps_tracks <-
do.call("rbind",
  lapply(active_states[, unique(animal_tag),], function(i) {
   # print(paste("animal_id:", i, " processing"))

    temp_active_states <- active_states[animal_tag == i,,]
    temp_activity_gaps <- activity_gaps[animal_tag == i,,]
    temp_gps <- data.table(gps[animal_tag == i & !is.na(longitude),,])

    deploy_start_ts <- trunc(temp_active_states[, min(to_active),], "mins")
    deploy_end_ts <- trunc(temp_active_states[, max(end_active),], "mins")
    #attr(deploy_start_ts, "tzone") <- NULL
    #attr(deploy_end_ts, "tzone") <- NULL

    animal_minutes <-
      data.table(minute = seq(deploy_start_ts, deploy_end_ts, by = "mins"),
                 active = 0)
    #attr(animal_minutes$minute, "tzone") <- NULL
    animal_minutes[,tmp_minute := minute]
    animal_minutes[,date := as.Date(minute)]

    setkey(temp_active_states, to_active, end_active)
    setkey(animal_minutes, minute, tmp_minute)

    animal_minutes <- data.table::foverlaps(animal_minutes, temp_active_states,
                                            type="within",
                                            by.x = c("minute", "tmp_minute"),
                                            by.y = c("to_active", "end_active"))

    animal_minutes[!is.na(duration), active := 1][
      ,c("to_active","end_active", "duration") := NA]

    # add NA to animal_minutes where no activity is available
    if (nrow(temp_activity_gaps) > 0) {
      for (j in 1:nrow(temp_activity_gaps)) {
        animal_minutes[
          minute >= temp_activity_gaps[j, to_NA, ] &
            minute <= temp_activity_gaps[j, end_NA, ],
          active := NA,]
        }
      }

    temp_pos <- matrix(unlist(hr_center[ hr_center$animal_tag == i,
                                         c(2,3)]),
                       nrow = 1)

  temp_tracks <- temp_gps[, .(track_id = 1:(nrow(temp_gps)),
                              animal_id,
                              tag_code,
                              ts_start = ts,
                              long_start = longitude,
                              lat_start = latitude,
                              ts_end = shift(ts, n = 1, type = "lead"),
                              long_end = shift(longitude, n = 1, type = "lead"),
                              lat_end = shift(latitude, n = 1, type = "lead")),]
  # remove last row that only contains a start position
  temp_tracks <- temp_tracks[!nrow(temp_tracks),,]
  temp_tracks$dist <- round(sp::spDists(as.matrix(temp_gps[,.(longitude, latitude)]), longlat=TRUE, segments = T)*1000)


# merge animal_minutes and tracks and sum active minutes per track_id
  setkey(temp_tracks, ts_start, ts_end)
  setkey(animal_minutes, minute, tmp_minute)
  animal_minutes <- foverlaps(animal_minutes[,.(active, minute, tmp_minute),],
                              temp_tracks[, .(track_id, ts_start, ts_end), ], type="within",
                              by.x = c("minute", "tmp_minute"),
                              by.y = c("ts_start", "ts_end"))

  sum_active_per_track <- animal_minutes[, .(active_minutes = sum(active)), by = track_id ]

  temp_tracks <- merge(temp_tracks, sum_active_per_track, by = "track_id", all.x = TRUE)


  return(temp_tracks)
  }
)
)

return(gps_tracks)}
