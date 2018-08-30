#' Calculate proportion of time in state active
#'
#' \code{states2prop_time_active} calculates the proportion of time in state
#'   active for each animal_tag and day.
#'
#' @param active_states A data.table with the predicted active states (return
#'   of function \code{\link{thresholds2states}}.
#' @param activity_gaps A data.table with the identified gaps in the activity
#'    data (return of function \code{\link{identify_activity_gaps}}.
#' @param gps A data.table with the GPS positions (at least one) of the animals.
#'   The table should contain the columns: <animal_tag>, <longitude>, <latitude>.
#' @param pos A vector containing longitude and latitude of the research area.
#'   This parameter is only necessary when no GPS-positions are provided or when
#'   the GPS-positions for some animals are missing.
#' @param dayshift Either "sunrise" or "dawn". Defines when consecutive days are
#'   separated so that each day contains one complete night and one complete day.
#'   The identifier for each day is called <date_se> for sunrise and <date_dawn>
#'   for dawn.
#' @param dawn_degree An integer between 0 and 90 that defines the angle of the
#'   sun below the horizon at dawn and dusk.
#' @param period Either "day" or "week", specifies the period of time in which the
#'   proportion of time in state active is aggregated.
#' @param max_na An integer with the maximum number of minutes with NA
#'   that are alloes for each period. When this threshold is crossed proportion of
#'   time in state active will be set to NA for this period.
#' @return A data.table with the proportion of time in state active for each
#'   animal_tag and day. The proportion of time in state active is returned for
#'   the whole day <total>, during daytime <day> (sunrise till sunset), during
#'   nighttime <night> (dusk till dawn), during dusk <dusk> (sunset till the
#'   sun reaches dawn_degree),during dawn <dawn> ()
#'
#' @examples states2prop_time_active(active_states = active_states_a,
#'                                   activity_gaps = activity_data_gaps,
#'                                   gps = gps_data,
#'                                   dayshift = "sunrise",
#'                                   dawn_degree = 12,
#'                                   period = "day"
#'                                   max_na = 30)

#'
#' @import data.table
#' @export

states2prop_time_active <- function(active_states,
                                    activity_gaps,
                                    gps = NULL,
                                    pos = NULL,
                                    dayshift = "sunrise",
                                    dawn_degree = 12,
                                    period = "day",
                                    max_na = 0) {


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
      if (!is.null(pos)) {
        hr_center <- rbind(hr_center,
                           data.frame(as.character(missing_gps),
                                      as.numeric(pos[1]),
                                      as.numeric(pos[2])),
                           use.names = FALSE)
      } else {
        stop(paste('GPS positions for animal_tag: ',
                   paste(missing_gps, collapse = ", "), ' are missing. Please provide the coordinates of the research area (parameter \'pos\') !'))
      }
    }
  } else if (all(is.na(pos))) {
    stop('You have to provide either a table with the GPS-positions
         or coordinates of the research area!')
  } else {
    hr_center <- data.frame("animal_tag" = animal_tags,
                            "median_longitude" = pos[1],
                            "median_latitude" = pos[2])}


# if (!is.null(gps)) {
#     hr_center <- gps[!is.na(longitude),
#                      .("median_longitude" = median(as.numeric(longitude)),
#                        "median_latitude" = median(as.numeric(latitude))),
#                       by = animal_tag]
#     } else if (all(is.na(pos))) {
#       stop('You have to provide either a table with the GPS-positions
#             or coordinates of the research area!')}

prop_time_active <-
do.call("rbind",
  lapply(active_states[, unique(animal_tag),], function(i) {
    print(paste("animal_id:", i, " processing"))

    temp_active_states <- active_states[animal_tag == i,,]
    temp_activity_gaps <- activity_gaps[animal_tag == i,,]

    deploy_start_ts <- trunc(temp_active_states[, min(to_active),], "mins")
    deploy_end_ts <- trunc(temp_active_states[, max(end_active),], "mins")

    animal_minutes <-
      data.table(minute = seq(deploy_start_ts, deploy_end_ts, by = "mins"),
                 active = 0)
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
    #
    # if (exists('hr_center') & nrow(hr_center[animal_tag == i ,,]) == 1) {
    #   temp_pos <- matrix(unlist(hr_center[animal_tag == i,
    #                                      .(median_longitude, median_latitude)]),
    #                   nrow = 1)
    #   } else if (!all(is.na(pos))) {
    #     temp_pos <- matrix(pos, nrow = 1)
    #    } else {
    #       stop('No GPS-position for this animal availabe to calculate
    #             time of day!')}

  day_seq <- as.POSIXct(seq(from = as.Date(deploy_start_ts) - lubridate::days(1),
                            to = as.Date(deploy_end_ts) + lubridate::days(1),
                            by = "days"))

  nighttime <- data.table(
    "ts_dawn" = maptools::crepuscule(temp_pos,
                                     day_seq,
                                     solarDep=c(dawn_degree),
                                     direction="dawn", POSIXct.out=TRUE)$time,
    "ts_sr" = maptools::crepuscule(temp_pos,
                                   day_seq,
                                   solarDep=c(0),
                                   direction="dawn",
                                   POSIXct.out=TRUE)$time,
    "ts_ss" = maptools::crepuscule(temp_pos,
                                   day_seq,
                                   solarDep=c(0),
                                   direction="dusk", POSIXct.out=TRUE)$time,
    "ts_dusk" = maptools::crepuscule(temp_pos,
                                     day_seq,
                                     solarDep=c(dawn_degree),
                                     direction="dusk", POSIXct.out=TRUE)$time,
    "ts_dawn_plusone" =
      maptools::crepuscule(temp_pos,
                           day_seq + lubridate::days(1),
                           solarDep=c(dawn_degree),
                           direction="dawn", POSIXct.out=TRUE)$time,
    "ts_sr_plusone" =
      maptools::crepuscule(temp_pos,
                           day_seq + lubridate::days(1),
                           solarDep=c(0),
                           direction="dawn", POSIXct.out=TRUE)$time)

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

  animal_minutes[, night := 1][, dawn := 0][,day := 0][, dusk := 0]
  animal_minutes[minute >= ts_dawn & minute < ts_dusk, night := 0][
                  minute >= ts_dawn_plusone, night := 0][
                  minute >= ts_dawn_plusone, dawn := 1][
                  minute >= ts_sr & minute < ts_ss, day := 1][
                  minute >= ts_ss & minute < ts_dusk, dusk := 1]
  animal_minutes <- animal_minutes[,.(minute,
                                      date_sr,
                                      night,
                                      dawn,
                                      day,
                                      dusk,
                                      active), ]

# remove rows with no date_sr (these occur at the first night when the
# animal was tagged after 0:00)
# animal_minutes <- animal_minutes[!is.na(date_sr),]


if (period == "day") {
  temp_prop_time_active <-
      animal_minutes[,.(night = round(sum((night == 1 & active == 1), na.rm=T) /
                                        sum(night==1),4),
                          day = round(sum((day == 1 & active == 1), na.rm=T) /
                                        sum(day==1),4),
                          dawn = round(sum((dawn == 1 & active == 1), na.rm=T) /
                                         sum(dawn==1),4),
                          dusk = round(sum((dusk == 1 & active == 1), na.rm=T) /
                                         sum(dusk==1),4),
                          day_twi = round(sum((night == 0 & active == 1), na.rm=T) /
                                            sum(night==0),4),
                          night_twi = round(sum((day == 0 & active == 1), na.rm=T) /
                                              sum(day==0),4),
                          total = round(sum(active == 1, na.rm =T)/.N,4),
                          n_na = sum(is.na(active)),
                          nr_mins = .N),
                       by = .(date_sr)]

  setkey(temp_prop_time_active, date_sr)
  setkey(nighttime, date_sr)
  temp_prop_time_active[nighttime, `:=` (ts_sr = i.ts_sr,
                                         ts_ss = i.ts_ss,
                                         ts_dusk = i.ts_dusk,
                                         ts_dawn = i.ts_dawn_plusone),]

  temp_prop_time_active <- temp_prop_time_active[!is.na(date_sr),, ]

  } else if (period == "week") {
    animal_minutes[, year_week := ts2yearweek(date_sr), ]
    temp_prop_time_active <-
      animal_minutes[,.(night = round(sum((night == 1 & active == 1), na.rm=T) /
                                        sum(night==1),4),
                        day = round(sum((day == 1 & active == 1), na.rm=T) /
                                      sum(day==1),4),
                        dawn = round(sum((dawn == 1 & active == 1), na.rm=T) /
                                       sum(dawn==1),4),
                        dusk = round(sum((dusk == 1 & active == 1), na.rm=T) /
                                       sum(dusk==1),4),
                        day_twi = round(sum((night == 0 & active == 1), na.rm=T) /
                                          sum(night==0),4),
                        night_twi = round(sum((day == 0 & active == 1), na.rm=T) /
                                            sum(day==0),4),
                        total = round(sum(active == 1, na.rm =T)/.N,4),
                        n_na = sum(is.na(active)),
                        nr_mins = .N),
                     by = .(year_week)]
  }

  temp_prop_time_active[, animal_tag := i, ]

  } else if (dayshift == "dawn") {

    setkey(nighttime, ts_dawn, ts_dawn_plusone)
    setkey(animal_minutes, minute, tmp_minute)
    animal_minutes <- foverlaps(animal_minutes, nighttime, type="within",
                                by.x = c("minute", "tmp_minute"),
                                by.y = c("ts_dawn", "ts_dawn_plusone"))

    animal_minutes[, night := 1][, dawn := 0][,day := 0][, dusk := 0]
    animal_minutes[minute >= ts_dawn & minute < ts_dusk, night := 0][
      minute >= ts_dawn & minute < ts_sr, dawn := 1][
        minute >= ts_sr & minute < ts_ss, day := 1][
          minute >= ts_ss & minute < ts_dusk, dusk := 1]
    animal_minutes <- animal_minutes[,.(minute, date_dawn, night, dawn, day,
                                        dusk, active)]

    # remove rows with no date_dawn (these occur at the first night when the
    # animal was tagged after 0:00)
    # animal_minutes <- animal_minutes[!is.na(date_dawn),]

  if (period == "day") {
    temp_prop_time_active <-
      animal_minutes[,.(night = round(sum((night == 1 & active == 1), na.rm=T) /
                                        sum(night==1),4),
                        day = round(sum((day == 1 & active == 1), na.rm=T) /
                                      sum(day==1),4),
                        dawn = round(sum((dawn == 1 & active == 1), na.rm=T) /
                                       sum(dawn==1),4),
                        dusk = round(sum((dusk == 1 & active == 1), na.rm=T) /
                                       sum(dusk==1),4),
                        day_twi = round(sum((night == 0 & active == 1), na.rm=T) /
                                          sum(night==0),4),
                        night_twi = round(sum((day == 0 & active == 1), na.rm=T) /
                                            sum(day==0),4),
                        total = round(sum(active == 1, na.rm =T)/.N,4),
                        n_na = sum(is.na(active)),
                        nr_mins = .N),
                     by = .(date_dawn)]

    setkey(temp_prop_time_active, date_dawn)
    setkey(nighttime, date_dawn)

    temp_prop_time_active[nighttime, `:=` (ts_dawn = i.ts_dawn, ts_sr = i.ts_sr,
                                           ts_ss = i.ts_ss, ts_dusk = i.ts_dusk)]
    temp_prop_time_active <- temp_prop_time_active[!is.na(date_dawn), ]

  } else if (period == "week") {
    animal_minutes[, year_week := ts2yearweek(date_dawn), ]
    temp_prop_time_active <-
      animal_minutes[,.(night = round(sum((night == 1 & active == 1), na.rm=T) /
                                        sum(night==1),4),
                        day = round(sum((day == 1 & active == 1), na.rm=T) /
                                      sum(day==1),4),
                        dawn = round(sum((dawn == 1 & active == 1), na.rm=T) /
                                       sum(dawn==1),4),
                        dusk = round(sum((dusk == 1 & active == 1), na.rm=T) /
                                       sum(dusk==1),4),
                        day_twi = round(sum((night == 0 & active == 1), na.rm=T) /
                                          sum(night==0),4),
                        night_twi = round(sum((day == 0 & active == 1), na.rm=T) /
                                            sum(day==0),4),
                        total = round(sum(active == 1, na.rm =T)/.N,4),
                        n_na = sum(is.na(active)),
                        nr_mins = .N),
                     by = .(year_week)]
    }

    temp_prop_time_active[, animal_tag := i, ]
  }

  # add NA to days where more than max_na (minutes) if activity were missing
  temp_prop_time_active[n_na > max_na,
                        `:=` (night=NA, day=NA, dawn=NA, dusk = NA, day_twi = NA,
                               night_twi = NA, total=NA), ]

  # add na to days where minutes are missing (mainly start- and end-days of
  # the observation period), usually a day has 1442 minutes, however, the
  # day_dawn is defined by the time of sunrise and thus has alternating number
  # of mins
  temp_prop_time_active[nr_mins < 1430, `:=` (night=NA, day=NA, dawn=NA, dusk=NA,
                                              day_twi=NA, night_twi=NA, total=NA)]

  temp_prop_time_active[, c("n_na", "nr_mins") := NULL]

  print("..done!")
  return(temp_prop_time_active)
  }
)
)

prop_time_active <- split_animaltag(prop_time_active)

return(prop_time_active)}
