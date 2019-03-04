#' Plot activity data
#'
#' \code{plot_activity} plots activity data against time of day for each day of
#'   the week.
#'
#' @param activity_data
#' @param thresholds
#' @param gps A data.table with the gps data.
#' @param act.available_act
#' @param states_a
#' @param states_b
#' @param states_c
#' @param select_animal_id
#'
#' @return
#' @example
#'
#' @import data.table
#' @export

plot_activity <- function (activity,
                           thresholds,
                           gps,
                           act.available_act,
                           states_a,
                           states_b,
                           states_c,
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
thresholds <- copy(thresholds)
gps <- copy(gps)
states_a <- copy(states_a)
states_b <- copy(states_b)
states_c <- copy(states_c)

# first plot option: no gps and no thresholds
################################################################################

if (all(is.na(gps)) & all(all(is.na(states_a)), all(is.na(states_b)), all(is.na(states_c)))) {
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
           xlab = "Time", ylab = "Activity", xaxt = "n",
           frame.plot = F, col = "blue")
      axis.POSIXct(1, at = seq(as.POSIXct(d_day),
                               as.POSIXct(d_day) + lubridate::days(1), by = "hour"))
      text(x = as.POSIXct(d_day) + lubridate::hours(20), y = d_ylim_max-d_ylim_max*0.1,
           labels = paste("Animal ID", ":  ", select_animal_id,
                          "\nDay:  ", d_day,
                          "\nGPS present:  ", gps_present,
                          sep= ""))
},
    act = manipulate::picker(as.list(act.available_act)),
    d_day = manipulate::picker(label = "day", as.list(as.character(unique(
      as.Date(activity[,ts]))))),
    d_ylim = manipulate::picker("max", "99th percentile", "95th percentile",
                                label = "ylim", initial = "99th percentile")
)
  } else if (!all(is.na(gps)) & all(all(is.na(states_a)), all(is.na(states_b)), all(is.na(states_c)))) {

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
         xlab = "Time", ylab = "Activity", xaxt = "n",
         frame.plot = F, col = "blue")
    axis.POSIXct(1, at = seq(as.POSIXct(d_day),
                             as.POSIXct(d_day) + lubridate::days(1), by = "hour"))
    text(x = as.POSIXct(d_day) + lubridate::hours(20), y = d_ylim_max-d_ylim_max*0.1,
         labels = paste("Animal ID", ":  ", select_animal_id,
                        "\nDay:  ", d_day,
                        "\nGPS present:  ", gps_present,
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
  },
  act = manipulate::picker(as.list(act.available_act)),
  d_day = manipulate::picker(label = "day", as.list(as.character(unique(
    as.Date(activity[,ts]))))),
  d_ylim = manipulate::picker("max", "99th percentile", "95th percentile",
                              label = "ylim", initial = "99th percentile"),
  plot_gps = manipulate::checkbox(initial = FALSE),
  plot_movement = manipulate::checkbox(initial = FALSE)
  )
} else if (all(is.na(gps)) & any(!all(is.na(states_a)), !all(is.na(states_b)), !all(is.na(states_c)))) {

# third plot option: gps and thresholds
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
         xlab = "Time", ylab = "Activity", xaxt = "n",
         frame.plot = F, col = "blue")
    axis.POSIXct(1, at = seq(as.POSIXct(d_day),
                             as.POSIXct(d_day) + lubridate::days(1), by = "hour"))
    text(x = as.POSIXct(d_day) + lubridate::hours(20), y = d_ylim_max-d_ylim_max*0.1,
         labels = paste("Animal ID", ":  ", select_animal_id,
                        "\nDay:  ", d_day,
                        "\nGPS present:  ", gps_present,
                        sep= ""))

  # plot predicted state
    if (plot_predicted_state == T) {
     temp_thresholds <-
        thresholds[threshold_period == ts2yearweek(temp_activity[1, ts, ]) ,,]

  if (!all(is.na(states_a))) {
      temp_active_states_a <- states_a[(as.Date(to_active) == d_day |
                                            as.Date(end_active) == d_day),,]
      for (i in 1:nrow(temp_active_states_a)){
        lines(c(temp_active_states_a[i,to_active],
                temp_active_states_a[i, end_active]),
              rep(temp_thresholds$threshold_a,2), lwd = 3, col = "orange")
      }
      abline(h = temp_thresholds$threshold_a, lty = 2, col = "orange")
  }
     if (!all(is.na(states_b))) {
       temp_active_states_b <- states_b[(as.Date(to_active) == d_day |
                                             as.Date(end_active) == d_day) ,,]
       for (i in 1:nrow(temp_active_states_b)){
         lines(c(temp_active_states_b[i,to_active],
                 temp_active_states_b[i, end_active]),
               rep(temp_thresholds$threshold_b,2), lwd = 3, col = "purple")
       }
       abline(h = temp_thresholds$threshold_b, lty = 2, col = "purple")
     }
     if (!all(is.na(states_c))) {
       temp_active_states_c <- states_c[(as.Date(to_active) == d_day |
                                             as.Date(end_active) == d_day) ,,]
       for (i in 1:nrow(temp_active_states_c)){
         lines(c(temp_active_states_c[i,to_active],
                 temp_active_states_c[i, end_active]),
               rep(temp_thresholds$threshold_c,2), lwd = 3, col = "cyan2")
       }
       abline(h = temp_thresholds$threshold_c, lty = 2, col = "cyan2")
     }
    }
  },
  act = manipulate::picker(as.list(act.available_act)),
  d_day = manipulate::picker(label = "day", as.list(as.character(unique(
    as.Date(activity[,ts]))))),
  d_ylim = manipulate::picker("max", "99th percentile", "95th percentile",
                              label = "ylim", initial = "99th percentile"),
  plot_predicted_state = manipulate::checkbox(initial = FALSE)
  )
} else {

# fourth plot option: gps and thresholds
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
       xlab = "Time", ylab = "Activity", xaxt = "n",
       frame.plot = F, col = "blue")
  axis.POSIXct(1, at = seq(as.POSIXct(d_day),
                           as.POSIXct(d_day) + lubridate::days(1), by = "hour"))
  text(x = as.POSIXct(d_day) + lubridate::hours(20), y = d_ylim_max-d_ylim_max*0.1,
       labels = paste("Animal ID", ":  ", select_animal_id,
                      "\nDay:  ", d_day,
                      "\nGPS present:  ", gps_present,
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
  # plot predicted state
  if (plot_predicted_state == T) {
    temp_thresholds <-
      thresholds[threshold_period == ts2yearweek(temp_activity[1, ts, ]) ,,]

    if (!all(is.na(states_a))) {
      temp_active_states_a <- states_a[(as.Date(to_active) == d_day |
                                          as.Date(end_active) == d_day),,]
      for (i in 1:nrow(temp_active_states_a)){
        lines(c(temp_active_states_a[i,to_active],
                temp_active_states_a[i, end_active]),
              rep(temp_thresholds$threshold_a,2), lwd = 3, col = "orange")
      }
      abline(h = temp_thresholds$threshold_a, lty = 2, col = "orange")
    }
    if (!all(is.na(states_b))) {
      temp_active_states_b <- states_b[(as.Date(to_active) == d_day |
                                          as.Date(end_active) == d_day) ,,]
      for (i in 1:nrow(temp_active_states_b)){
        lines(c(temp_active_states_b[i,to_active],
                temp_active_states_b[i, end_active]),
              rep(temp_thresholds$threshold_b,2), lwd = 3, col = "purple")
      }
      abline(h = temp_thresholds$threshold_b, lty = 2, col = "purple")
    }
    if (!all(is.na(states_c))) {
      temp_active_states_c <- states_c[(as.Date(to_active) == d_day |
                                          as.Date(end_active) == d_day) ,,]
      for (i in 1:nrow(temp_active_states_c)){
        lines(c(temp_active_states_c[i,to_active],
                temp_active_states_c[i, end_active]),
              rep(temp_thresholds$threshold_c,2), lwd = 3, col = "cyan2")
      }
      abline(h = temp_thresholds$threshold_c, lty = 2, col = "cyan2")
    }
  }
},
act = manipulate::picker(as.list(act.available_act)),
d_day = manipulate::picker(label = "day", as.list(as.character(unique(
  as.Date(activity[,ts]))))),
d_ylim = manipulate::picker("max", "99th percentile", "95th percentile",
                            label = "ylim", initial = "99th percentile"),
plot_gps = manipulate::checkbox(initial = FALSE),
plot_movement = manipulate::checkbox(initial = FALSE),
plot_predicted_state = manipulate::checkbox(initial = FALSE)
)
}
}
