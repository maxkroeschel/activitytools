#' Plot activity data
#'
#' \code{plot_activity} plots activity data against time of day for each day of
#'   the week.
#'
#' @param activity A data.table with the activity data. The following columns
#'   should be present: 'animal_tag' and 'ts'.
#' @param gps A data.table with the gps data.
#' @param thresholds
#' @param animal_id
#' @param tag_code
#'
#' @return
#' @examples
#' plot_activity(activity = activity_data,
#'               animal_id = '12',
#'               gps = gps_data,
#'               thresholds = activity_threshold_final)
#'
#' @import data.table
#' @export

plot_activity <- function (activity,
                           thresholds,
                           gps = NULL,
                           animal_id = NULL,
                           tag_code = NULL) {

  if(all(is.na(gps))){
    gps <- NULL
  }

if (!is.null(animal_id)) {ident <- "animal_id"
                          ident_value <- animal_id
                          } else
  if (!is.null(tag_code)) {ident <- "tag_code"
                           ident_value <- tag_code
                           } else {
    stop('You have to provide either animal_id or tag_code!')
                             }

activity <- activity[get(ident) == ident_value,,]

#!!! insert check if ident is present in gps
if (is.null(gps)) {
  gps_present = "no"
} else if (gps[get(ident) == ident_value & !is.na(longitude), .N,] == 0) {
  gps_present = "no"
    } else {
      gps_present <- "yes"
      hr_center <- matrix(c(gps[get(ident) == ident_value &
                                !is.na(longitude),
                              median(as.numeric(longitude)),],
                          gps[get(ident) == ident_value  &
                                !is.na(longitude),
                              median(as.numeric(latitude)),]),
                        nrow = 1)
    }


manipulate::manipulate({
  temp_activity <- activity[as.Date(ts) == d_day,,]
#  d_week <- as.character(unique(temp_activity[,week,]))

if (gps_present != "no") {
  if (gps[get(ident) == ident_value &
        as.Date(ts) == d_day &
        !is.na(longitude),.N,] == 0 ) {
          gps_present <- "not for this day"
    }
  }

if (axis == "act_xy"){
  if (!("act_xy" %in% colnames(activity))) {
    message("There is no column <act_xy> present!")
  }
  d_ylim_max <- 350
  plot(x = temp_activity[,ts],
       y = temp_activity[,get(axis)],
       type = 'h',
       xlim = c(as.POSIXct(d_day, format = "%Y-%m-%d"),
                as.POSIXct(d_day, format = "%Y-%m-%d") + lubridate::days(1)),
       ylim = c(0,d_ylim_max),
       xlab = "time", ylab = axis, xaxt = "n",
       frame.plot = F, col = "blue")
  axis.POSIXct(1, at = seq(as.POSIXct(d_day),
                           as.POSIXct(d_day) + lubridate::days(1), by = "hour"))
  text(x = as.POSIXct(d_day) + lubridate::hours(20), y = 300,
       labels = paste(ident, ":  ", ident_value,
                      "\nday:  ", d_day,
                      #"\nweek:  ",d_week,
                       "\ngps present:  ", gps_present,
                        sep= ""))
  } else {
    d_ylim_max <- 200
    plot(temp_activity[,ts], temp_activity[,get(axis)],
          type = 'h', col = "blue",
          xlim = c(as.POSIXct(d_day, format = "%Y-%m-%d"),
                   as.POSIXct(d_day, format = "%Y-%m-%d") + lubridate::days(1)),
                  ylim = c(0, d_ylim_max),
                  xlab = "time", ylab = axis,
                  frame.plot = F, xaxt = "n")
    axis.POSIXct(1, at = seq(as.POSIXct(d_day),
                             as.POSIXct(d_day) + lubridate::days(1),
                             by = "hour"))
    text(x = as.POSIXct(d_day) + lubridate::hours(20), y = 180,
        labels = paste(ident, ":  ", ident_value,
                        "\nday:  ", d_day,
 #                      "\nweek:  ",d_week,
                        "\ngps present:  ", gps_present,
                        sep= ""))
    }

if  (gps_present != "no"){
  dawn <- maptools::crepuscule(hr_center,  as.POSIXct(d_day, tz="UTC"),
                               solarDep=c(12,0), direction="dawn",
                               POSIXct.out=TRUE)
  dusk <- maptools::crepuscule(hr_center,  as.POSIXct(d_day, tz="UTC"),
                               solarDep=c(12,0), direction="dusk",
                               POSIXct.out=TRUE)
  lines(x = c(dawn$time), y = rep(-7,2), col = "orange",
        lwd = 5)
  lines(x = c(dusk$time), y = rep(-7,2), col = "orange",
        lwd = 5)
  }


# add GPS-points
if (plot_gps == TRUE & gps_present == "yes") {
  temp_gps <- gps[get(ident) == ident_value &
                    as.Date(ts) == d_day &
                    !is.na(longitude) ,]
    points(temp_gps$ts, rep(-4,length(temp_gps$ts)),
            pch = 19, cex = 0.7, col = "green")
    }

# add movement (spatial displacement)
if (plot_movement == T & gps_present == "yes") {
  temp_gps <- gps[get(ident) == ident_value &
                    as.Date(ts) == d_day &
                    !is.na(longitude) ,]
  ll <- cbind(temp_gps$longitude, temp_gps$latitude)
  distance <- sp::spDists(ll, longlat=TRUE, segments = T)*1000
  for (i in 1:(nrow(temp_gps)-1)){
    lines(temp_gps$ts[c(i,i+1)], rep(min(distance[i],d_ylim_max), times = 2),
          col = "green")
    }
  points(x = head(temp_gps$ts,-1)[distance > d_ylim_max],
         y = rep(d_ylim_max, sum(distance > d_ylim_max)),
         col = "green", pch = 17)
  }

# plot moving average
if (plot_moving_window == TRUE) {
  lines(temp_activity[,ts,],
       zoo::rollapply(temp_activity[,get(axis)],
                      width = width_axis_ma,
                      FUN = function(x) mean(x, na.rm =T),
                 partial = T, align = "center"),
       col = "red",
       lwd = 2)
  }

# plot state
if (plot_state == T) {
  temp_active_states <- activity2states(activity = temp_activity,
                                        axis = axis,
                                        axis_ma = 'no',
                                        width_axis_ma = width_axis_ma,
                                        threshold = threshold,
                                        min_duration_active_state = min_duration_active_state)

  if (nrow(temp_active_states) > 0) {
    for (i in 1:nrow(temp_active_states)){
      lines(c(temp_active_states[i,to_active],
              temp_active_states[i, end_active]),
              rep(threshold,2), lwd = 5)}}
  }
 abline(h = threshold, lty = 2)

# plot predicted state
if (plot_predicted_state == T) {
    temp_thresholds <-
    thresholds[get(ident) == ident_value &
                              threshold_period == ts2yearweek(temp_activity[1, ts, ]) ,,]

  # plot temp_active_states for threshold_a
  temp_active_states <-
    activity2states(activity = temp_activity,
                    axis = temp_thresholds[,axis,],
                    axis_ma = temp_thresholds[,axis_ma,],
                    threshold = temp_thresholds[,threshold_a,],
                    min_duration_active_state = temp_thresholds[,min_duration_active_state,])

        for (i in 1:nrow(temp_active_states)){
          lines(c(temp_active_states[i,to_active],
                  temp_active_states[i, end_active]),
                rep(temp_thresholds$threshold_a,2), lwd = 3, col = "orange")
        }
        abline(h = temp_thresholds$threshold_a, lty = 2, col = "orange")

  # plot temp_active_states for threshold_b
    temp_active_states <-
        activity2states(activity = temp_activity,
                        axis = temp_thresholds[,axis,],
                        axis_ma = temp_thresholds[,axis_ma,],
                        threshold = temp_thresholds[,threshold_b,],
                        min_duration_active_state = temp_thresholds[,min_duration_active_state,])

        for (i in 1:nrow(temp_active_states)){
          lines(c(temp_active_states[i,to_active],
                  temp_active_states[i, end_active]),
                rep(temp_thresholds$threshold_b,2)+0.5, lwd = 3, col = "purple")
        }
        abline(h = temp_thresholds$threshold_b+0.5, lty = 2, col = "purple")

  # plot temp_active_states for threshold_c
    temp_active_states <-
        activity2states(activity = temp_activity,
                        axis = temp_thresholds[,axis,],
                        axis_ma = temp_thresholds[,axis_ma,],
                        threshold = temp_thresholds[,threshold_c,],
                        min_duration_active_state = temp_thresholds[,min_duration_active_state,])

        for (i in 1:nrow(temp_active_states)){
          lines(c(temp_active_states[i,to_active],
                  temp_active_states[i, end_active]),
                rep(temp_thresholds$threshold_c,2)-0.5, lwd = 3, col = "green")
        }
        abline(h = temp_thresholds$threshold_c-0.5, lty = 2, col = "green")
        }
     },
  axis = manipulate::picker("act_x", "act_y", "act_xy"),
  d_day = manipulate::picker(label = "day", as.list(as.character(unique(
    as.Date(activity[,ts]))))),
  plot_gps = manipulate::checkbox(initial = FALSE),
  plot_movement = manipulate::checkbox(initial = FALSE),
  plot_moving_window = manipulate::checkbox(initial = FALSE),
  plot_state  = manipulate::checkbox(initial = FALSE),
  width_axis_ma = manipulate::slider(min = 1, max = 60, step = 1, initial = 3),
  threshold = manipulate::slider(min = 0, max = 150, step = 1, initial = 0),
  min_duration_active_state = manipulate::slider(min = 0, max = 150, step = 5, initial = 5),
  plot_predicted_state  = manipulate::checkbox(initial = FALSE))
}
