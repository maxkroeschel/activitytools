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
#' @param dayshift Either "sunrise" or "dawn". Defines when consecutive days are
#'   separated so that each day contains one complete night and one complete day.
#'   The identifier for each day is called <date_sr> for sunrise and <date_dawn>
#'   for dawn.
#' @param dawn_degree An integer between 0 and 90 that defines the angle of the
#'   sun below the horizon at dawn and dusk.
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
                          gps = NULL,
                          dayshift = "sunrise",
                          dawn_degree = 12) {


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


# calculate sunpositions and add these information to the gps_tracks

  day_seq <- as.POSIXct(seq(from = as.Date(deploy_start_ts) - lubridate::days(1),
                            to = as.Date(deploy_end_ts) + lubridate::days(1),
                            by = "days"))

  nighttime <- data.table(
    "ts_dawn" = suntools::crepuscule(temp_pos,
                                     day_seq,
                                     solarDep=c(dawn_degree),
                                     direction="dawn", POSIXct.out=TRUE)$time,
    "ts_sr" = suntools::crepuscule(temp_pos,
                                   day_seq,
                                   solarDep=c(0),
                                   direction="dawn",
                                   POSIXct.out=TRUE)$time,
    "ts_ss" = suntools::crepuscule(temp_pos,
                                   day_seq,
                                   solarDep=c(0),
                                   direction="dusk", POSIXct.out=TRUE)$time,
    "ts_dusk" = suntools::crepuscule(temp_pos,
                                     day_seq,
                                     solarDep=c(dawn_degree),
                                     direction="dusk", POSIXct.out=TRUE)$time,
    "ts_dawn_plusone" =
      suntools::crepuscule(temp_pos,
                           day_seq + lubridate::days(1),
                           solarDep=c(dawn_degree),
                           direction="dawn", POSIXct.out=TRUE)$time,
    "ts_sr_plusone" =
      suntools::crepuscule(temp_pos,
                           day_seq + lubridate::days(1),
                           solarDep=c(0),
                           direction="dawn", POSIXct.out=TRUE)$time)

  if (Sys.getenv("TZ") == 'UTC' & is.null(attr(nighttime$ts_dawn, "tzone"))) {
    attr(nighttime$ts_dawn, "tzone") <- 'UTC'
    attr(nighttime$ts_sr, "tzone") <- 'UTC'
    attr(nighttime$ts_ss, "tzone") <- 'UTC'
    attr(nighttime$ts_dusk, "tzone") <- 'UTC'
    attr(nighttime$ts_dawn_plusone, "tzone") <- 'UTC'
    attr(nighttime$ts_dusk, "tzone") <- 'UTC'
    attr(nighttime$ts_sr_plusone, "tzone") <- 'UTC'
  }

  nighttime[,date_dawn := as.Date(ts_dawn)]
  nighttime[,date_sr := as.Date(ts_sr)]

  # Check for days when the sun did not set below dawn_degree. These days will be
  # removed from the table.
  if (nighttime[(is.na(ts_dawn) | is.na(ts_dawn_plusone)), .N,] > 0) {
    days_with_no_night <- animal_minutes[,unique(date),][
      nighttime[,(is.na(ts_dawn) | is.na(ts_dawn_plusone)),]]
    print(paste("!!! The deployment period of this animals covers days at which the sun did not set below 'dawn_degree'. The following days were removed from the table:", paste(days_with_no_night, collapse  = ", "), sep = " "))
    #    animal_minutes <- animal_minutes[!(date %in% days_with_no_night),,]
    nighttime <- nighttime[!(is.na(ts_dawn) | is.na(ts_dawn_plusone)),,]
  }

  if (dayshift == "sunrise") {
    setkey(nighttime, ts_sr, ts_sr_plusone)
    setkey(animal_minutes, minute, tmp_minute)
    animal_minutes <- foverlaps(animal_minutes, nighttime, type="within",
                                by.x = c("minute", "tmp_minute"),
                                by.y = c("ts_sr", "ts_sr_plusone"))

    animal_minutes[, tod := "night"][
        minute >= ts_dawn_plusone, tod := "dawn"][
          minute >= ts_sr & minute < ts_ss, tod := "day"][
            minute >= ts_ss & minute < ts_dusk, tod := "dusk"]
    animal_minutes <- animal_minutes[!is.na(track_id),,]
    sum_active_per_track <- animal_minutes[, .(active_minutes = sum(active),
                                               tod = names(sort(table(tod), decreasing = TRUE)[1]),
                                               date_sr = names(sort(table(date_sr), decreasing = TRUE)[1])),
                                           by = track_id ]
    } else
      if (dayshift == "dawn") {
        setkey(nighttime, ts_dawn, ts_dawn_plusone)
        setkey(animal_minutes, minute, tmp_minute)
        animal_minutes <- foverlaps(animal_minutes, nighttime, type="within",
                                    by.x = c("minute", "tmp_minute"),
                                    by.y = c("ts_dawn", "ts_dawn_plusone"))

        animal_minutes[, tod := "night"][
          minute >= ts_dawn & minute < ts_sr, tod := "dawn"][
            minute >= ts_sr & minute < ts_ss, tod := "day"][
              minute >= ts_ss & minute < ts_dusk, tod := "dusk"]
        animal_minutes <- animal_minutes[!is.na(track_id),,]
        sum_active_per_track <-
          animal_minutes[, .(active_minutes = sum(active),
                             tod = names(sort(table(tod), decreasing = TRUE)[1]),
                             tod_span = paste(sort(unique(tod)), collapse = " "),
                             date_dawn = names(sort(table(date_dawn), decreasing = TRUE)[1])),
                                               by = track_id ]}

  temp_tracks <- merge(temp_tracks, sum_active_per_track, by = "track_id", all.x = TRUE)


  return(temp_tracks)
  }
))

return(gps_tracks)}
