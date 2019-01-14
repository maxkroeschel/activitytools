#' Plot activity data
#'
#' \code{plot_activity} plots activity data against time of day for each day of
#'   the week.
#'
#' @param activity_data
#' @param gps A data.table with the gps data.
#' @param act.available_act
#' @param select_animal_id
#'
#' @return
#' @example
#'
#' @import data.table
#' @export

plot_activity_eval <- function (activity,
                                gps,
                                act.available_act,
                                select_animal_id = NULL) {

  if (all(is.na(gps))) {
    gps_present = "no"
  } else if (gps[!is.na(longitude), .N,] == 0) {
    gps_present = "no"
  } else {
    gps_present <- "yes"
    hr_center <- matrix(c(gps[!is.na(longitude),
                              median(as.numeric(longitude)),],
                          gps[!is.na(longitude),
                              median(as.numeric(latitude)),]),
                        nrow = 1)
  }

  # workaround: copy data.tables to improve performance of manipulate ()
  activity <- copy(activity)
  gps <- copy(gps)

  # first plot option: no gps and no thresholds
  ################################################################################

if (all(is.na(gps))) {
  manipulate::manipulate({
    temp_activity <- activity[as.Date(ts) == d_day,,]

    if (gps_present != "no") {
      if (gps[as.Date(ts) == d_day &
              !is.na(longitude),.N,] == 0 ) {
        gps_present <- "not for this day"
      }
    }

    if (d_ylim == "max") {
      d_ylim_max <- max(activity[,get(act),], na.rm = TRUE)
    } else if (d_ylim == "99th percentile") {
      d_ylim_max <- quantile(activity[,get(act),], 0.99, na.rm = TRUE) +
        quantile(activity[,get(act),], 0.99, na.rm = TRUE)*0.1
    } else {
      d_ylim_max <- quantile(activity[,get(act),], 0.95, na.rm = TRUE) +
        quantile(activity[,get(act),], 0.95, na.rm = TRUE)*0.1
    }

    plot(x = temp_activity[,ts],
         y = temp_activity[,get(act)],
         type = 'h',
         xlim = c(as.POSIXct(d_day, format = "%Y-%m-%d"),
                  as.POSIXct(d_day, format = "%Y-%m-%d") + lubridate::days(1)),
         ylim = c(0,d_ylim_max),
         xlab = "time", ylab = act, xaxt = "n",
         frame.plot = F, col = "blue")
    axis.POSIXct(1, at = seq(as.POSIXct(d_day),
                             as.POSIXct(d_day) + lubridate::days(1), by = "hour"))
    text(x = as.POSIXct(d_day) + lubridate::hours(20), y = d_ylim_max-d_ylim_max*0.1,
         labels = paste("animal_id", ":  ", select_animal_id,
                        "\nday:  ", d_day,
                        #"\nweek:  ",d_week,
                        "\ngps present:  ", gps_present,
                        sep= ""))

# plot moving average
  if (plot_moving_window == TRUE) {
      lines(temp_activity[,ts,],
           zoo::rollapply(temp_activity[,get(act),],
                          width = width_ma,
                          FUN = function(x) mean(x, na.rm =T),
                     partial = T, align = "center"),
           col = "red",
           lwd = 1.5)
  }

# plot state
  if (plot_state == T) {
    temp_active_states <- activity2states(activity = temp_activity,
                                          act = act,
                                          act_ma = 'no',
                                          act.width_ma = width_ma,
                                          threshold = threshold,
                                          min_duration_active_state = min_duration_active)

    if (nrow(temp_active_states) > 0) {
      for (i in 1:nrow(temp_active_states)){
        lines(c(temp_active_states[i,to_active],
                temp_active_states[i, end_active]),
                rep(threshold,2), lwd = 5, col = "black")}}
    }
   abline(h = threshold, lty = 2, col = "black")
},
  act = manipulate::picker(as.list(act.available_act)),
  d_day = manipulate::picker(label = "day", as.list(as.character(unique(
    as.Date(activity[,ts]))))),
  d_ylim = manipulate::picker("max", "99th percentile", "95th percentile",
                              label = "ylim", initial = "99th percentile"),
  plot_moving_window = manipulate::checkbox(initial = FALSE),
  plot_state  = manipulate::checkbox(initial = FALSE),
  width_ma = manipulate::slider(min = 1, max = 60, step = 1, initial = 3),
  threshold = manipulate::slider(min = 0, max = 150, step = 1, initial = 0),
  min_duration_active = manipulate::slider(min = 0, max = 150, step = 1, initial = 5)
)
  } else if (!all(is.na(gps))) {

    # second plot option: gps and no thresholds
    ################################################################################

    manipulate::manipulate({
      temp_activity <- activity[as.Date(ts) == d_day,,]

      if (gps_present != "no") {
        if (gps[as.Date(ts) == d_day &
                !is.na(longitude),.N,] == 0 ) {
          gps_present <- "not for this day"
        }
      }

      if (d_ylim == "max") {
        d_ylim_max <- max(activity[,get(act),], na.rm = TRUE)
      } else if (d_ylim == "99th percentile") {
        d_ylim_max <- quantile(activity[,get(act),], 0.99, na.rm = TRUE) +
          quantile(activity[,get(act),], 0.99, na.rm = TRUE)*0.1
      } else {
        d_ylim_max <- quantile(activity[,get(act),], 0.95, na.rm = TRUE) +
          quantile(activity[,get(act),], 0.95, na.rm = TRUE)*0.1
      }

      plot(x = temp_activity[,ts],
           y = temp_activity[,get(act)],
           type = 'h',
           xlim = c(as.POSIXct(d_day, format = "%Y-%m-%d"),
                    as.POSIXct(d_day, format = "%Y-%m-%d") + lubridate::days(1)),
           ylim = c(0,d_ylim_max),
           xlab = "time", ylab = act, xaxt = "n",
           frame.plot = F, col = "blue")
      axis.POSIXct(1, at = seq(as.POSIXct(d_day),
                               as.POSIXct(d_day) + lubridate::days(1), by = "hour"))
      text(x = as.POSIXct(d_day) + lubridate::hours(20), y = d_ylim_max-d_ylim_max*0.1,
           labels = paste("animal_id", ":  ", select_animal_id,
                          "\nday:  ", d_day,
                          #"\nweek:  ",d_week,
                          "\ngps present:  ", gps_present,
                          sep= ""))

      # add sunrise and sunset
      dawn <- maptools::crepuscule(hr_center,  as.POSIXct(d_day, tz="UTC"),
                                   solarDep=c(12,0), direction="dawn",
                                   POSIXct.out=TRUE)
      dusk <- maptools::crepuscule(hr_center,  as.POSIXct(d_day, tz="UTC"),
                                   solarDep=c(12,0), direction="dusk",
                                   POSIXct.out=TRUE)
      lines(x = c(dawn$time), y = rep(-(d_ylim_max/50),2), col = "grey",
            lwd = 7)
      lines(x = c(dusk$time), y = rep(-(d_ylim_max/50),2), col = "grey",
            lwd = 7)

      # add GPS-points
      if (plot_gps == TRUE & gps_present == "yes") {
        temp_gps <- gps[as.Date(ts) == d_day &
                          !is.na(longitude) ,]
        points(temp_gps$ts, rep(-(d_ylim_max/50),length(temp_gps$ts)),
               pch = 19, cex = 0.7, col = "black")
      }
      # add movement (spatial displacement)
      if (plot_movement == T & gps_present == "yes") {
        temp_gps <- gps[as.Date(ts) == d_day &
                          !is.na(longitude) ,]
        ll <- cbind(temp_gps$longitude, temp_gps$latitude)
        distance <- sp::spDists(ll, longlat=TRUE, segments = T)*1000
        for (i in 1:(nrow(temp_gps)-1)){
          lines(temp_gps$ts[c(i,i+1)], rep(min(distance[i],d_ylim_max), times = 2),
                col = "black")
        }
        points(x = head(temp_gps$ts,-1)[distance > d_ylim_max],
               y = rep(d_ylim_max, sum(distance > d_ylim_max)),
               col = "black", pch = 17)
      }

      # plot moving average
      if (plot_moving_window == TRUE) {
        lines(temp_activity[,ts,],
              zoo::rollapply(temp_activity[,get(act),],
                             width = width_ma,
                             FUN = function(x) mean(x, na.rm =T),
                             partial = T, align = "center"),
              col = "red",
              lwd = 1.5)
      }

      # plot state
      if (plot_state == T) {
        temp_active_states <- activity2states(activity = temp_activity,
                                              act = act,
                                              act_ma = 'no',
                                              act.width_ma = width_ma,
                                              threshold = threshold,
                                              min_duration_active_state = min_duration_active)

        if (nrow(temp_active_states) > 0) {
          for (i in 1:nrow(temp_active_states)){
            lines(c(temp_active_states[i,to_active],
                    temp_active_states[i, end_active]),
                  rep(threshold,2), lwd = 5, col = "black")}}
      }
      abline(h = threshold, lty = 2, col = "black")
    },
    act = manipulate::picker(as.list(act.available_act)),
    d_day = manipulate::picker(label = "day", as.list(as.character(unique(
      as.Date(activity[,ts]))))),
    d_ylim = manipulate::picker("max", "99th percentile", "95th percentile",
                                label = "ylim", initial = "99th percentile"),
    plot_gps = manipulate::checkbox(initial = FALSE),
    plot_movement = manipulate::checkbox(initial = FALSE),
    plot_moving_window = manipulate::checkbox(initial = FALSE),
    plot_state  = manipulate::checkbox(initial = FALSE),
    width_ma = manipulate::slider(min = 1, max = 60, step = 1, initial = 3),
    threshold = manipulate::slider(min = 0, max = 150, step = 1, initial = 0),
    min_duration_active = manipulate::slider(min = 0, max = 150, step = 1, initial = 5)
    )
  }
}
