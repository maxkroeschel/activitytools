###############################################################################
# 99_f_plot_activity_state
# author: Max Kroeschel
# info: Function plots activity and state over time
################################################################################
# execute with:   plot_activity_data.f(animal_id = 12)

# axis <- "act_xy"
# animal_id = 64
# plot_state = TRUE
# window_width_activity_ma = 2
# threshold = 10
# d_day = "2007-04-05"
# min_duration_active_state = 10

  require("data.table")
  require("maptools")
  require("manipulate")
  source("/home/max/Dropbox/work/state_detection/code/99_f_activity2states.R")

plot_activity <- function (activity,
                           gps = NULL,
                           animal_id = NULL,
                           tag_code = NULL,
                           axis,  plot_gps, plot_movement, 
                           plot_moving_window, plot_state, 
                           window_width_activity_ma, 
                           threshold,
                           min_duration_active_state,
                           plot_predicted_state){
  
  if (!is.null(animal_id)) {ident <- "animal_id"
                            ident_value <- animal_id} else 
    if (!is.null(tag_code)) {ident <- "tag_code"
                             ident_value <- tag_code} else {
      stop('You have to provide either an animal_id or a tag_code!')}

  
  activity <- activity[get(ident) == ident_value,,]

  manipulate({
    d_activity <- activity[as.Date(ts) == d_day & 
                                         animal_id == animal_id,]
  #  d_week <- as.character(unique(d_activity[,week,]))
    
    # check if gps-data of this animal are present
    
    if (is.null(gps)) {gps_present <- "no"} else 
      if (gps[animal_id == animal_id &
                   !is.na(longitude),.N,] == 0 ) {gps_present <- "no"} else 
        if (gps[animal_id == animal_id & 
                   as.Date(ts) == d_day & 
                   !is.na(longitude),.N,] >0 ) {gps_present <- "yes"} else
         (gps_present <- "not for this day")
    
    if (axis == "act_xy"){
      d_ylim_max <<- 350
      plot(d_activity[,ts], d_activity[,get(axis)], 
           type = 'h', xlim = c(as.POSIXct(d_day, format = "%Y-%m-%d"), 
                                as.POSIXct(d_day, format = "%Y-%m-%d") + days(1)),  
           ylim = c(0,d_ylim_max), xlab = "time", ylab = axis, frame.plot = F, xaxt = "n",
           col = "blue")
      axis.POSIXct(1, at = seq(as.POSIXct(d_day), 
                               as.POSIXct(d_day)+days(1), by = "hour"))
      text(x = as.POSIXct(d_day)+hours(20), y = 320, 
           labels = paste("animal_id:  ", animal_id,
                          "\nday:  ", d_day,
                      #    "\nweek:  ",d_week,
                          "\ngps present:  ", gps_present,
                          sep= ""))
      } else {
             d_ylim_max <<- 200
             plot(d_activity[,ts], d_activity[,get(axis)], 
                  type = 'h', xlim = c(as.POSIXct(d_day, format = "%Y-%m-%d"), 
                                       as.POSIXct(d_day, format = "%Y-%m-%d") + days(1)),  
                  ylim = c(0,d_ylim_max), xlab = "time", ylab = axis, frame.plot = F, xaxt = "n",
                  col = "blue")
             axis.POSIXct(1, at = seq(as.POSIXct(d_day), 
                                      as.POSIXct(d_day)+days(1), by = "hour"))
             text(x = as.POSIXct(d_day)+hours(20), y = 180, 
                  labels = paste("animal_id:  ", animal_id,
                                 "\nday:  ", d_day,
 #                                "\nweek:  ",d_week,
                                 "\ngps present:  ", gps_present,
                                 sep= ""))}       
    
    if  (gps_present != "no"){
    
    # extract the positions of d_day  
    d_gps <- gps[as.Date(ts) == d_day & 
                                 animal_id == animal_id & 
                                 !is.na(longitude) ,]
    # calculate center of homerange
    hr_center <- matrix(c(gps[animal_id == animal_id & !is.na(longitude), 
                                   median(as.numeric(longitude)),],
                          gps[animal_id == animal_id & !is.na(longitude), 
                                   median(as.numeric(latitude)),]), 
                          nrow = 1)
      
    dawn <- crepuscule(hr_center,  as.POSIXct(d_day, tz="UTC"), solarDep=c(12,0), 
                       direction="dawn", POSIXct.out=TRUE)
    dusk <- crepuscule(hr_center,  as.POSIXct(d_day, tz="UTC"), solarDep=c(12,0), 
                       direction="dusk", POSIXct.out=TRUE)
    lines(x = c(dawn$time), y = rep(-7,2), col = "orange", 
          lwd = 5)
    lines(x = c(dusk$time), y = rep(-7,2), col = "orange", 
          lwd = 5)
    
    
    d_gps <- gps[as.Date(ts) == d_day & 
                               animal_id == animal_id & 
                               !is.na(longitude) ,]
    
    # add GPS-points  
    if (plot_gps == TRUE & gps_present == "yes") {
       points(d_gps$ts, 
             rep(-4,length(d_gps$ts)), 
             pch = 19, cex = 0.7, col = "green")}
    
    # add movement (spatial displacement)
    if (plot_movement == T & gps_present == "yes") {
      ll <- cbind(d_gps$longitude, d_gps$latitude)
      distance <- spDists(ll, longlat=TRUE, segments = T)*1000
      for (i in 1:(nrow(d_gps)-1)){
        lines(d_gps$ts[c(i,i+1)], rep(min(distance[i],d_ylim_max), times = 2),
              col = "green")}
      points(x = head(d_gps$ts,-1)[distance > d_ylim_max],
             y = rep(d_ylim_max, sum(distance > d_ylim_max)),
             col = "green", pch = 17)}
      }
    
    
    # plot moving average 
    if (plot_moving_window == TRUE) {
      lines(d_activity[,ts,], 
           rollapply(d_activity[,get(axis)], width = window_width_activity_ma, FUN = function(x) mean(x, na.rm =T), 
                     partial = T, align = "center"), 
           col = "red", 
           lwd = 2)}
    
    # plot state
    if (plot_state == T) {
      temp_active_states <- activity2states.f(activity = d_activity, 
                                           # data_gaps = activity_data_gaps,             
                                            axis = axis,
                                            axis_ma = 'no',
                                            window_width_axis_ma = window_width_activity_ma,
                                            threshold = threshold,
                                            min_duration_active_state = min_duration_active_state)
                    
      if (nrow(temp_active_states) > 0) {
      for (i in 1:nrow(temp_active_states)){
        lines(c(temp_active_states[i,to_active],
                temp_active_states[i, end_active]), 
              rep(threshold,2), lwd = 5)}}
        # text(x = mean(data_t$ts[trunc(rowMeans(temp_active_states[["state_mat"]][i,c(1,2)]))]), y = threshold, label = temp_active_states[["state_mat"]][i,3], pos = 3)
      }
    abline(h = threshold, lty = 2)
    
    # plot predicted state
    if (plot_predicted_state == T) {
      d_thresholds <- activity_thresholds_final[animal_id == animal_id & period == d_week,,]
      # plot temp_active_states for threshold_a
        
      #temp_active_states <- activity2state.f(parameter = d_activity[,get(axis)], 
      #                                   parameter_ma = "no",
      #                                   ts = d_activity$ts, 
      #                                   2, d_thresholds$threshold_a)[["active_states"]]
        
      temp_active_states <- activity2states.f(data = d_activity, 
                                             activity_data_gaps = activity_data_gaps,             
                                             activity = axis,
                                             activity_ma = 'no',
                                             window_width_activity_ma = 2,
                                             threshold = d_thresholds$threshold_a,
                                             min_duration_active_state = min_duration_active_state) 
        
        
        
        for (i in 1:nrow(temp_active_states)){
          lines(c(temp_active_states[i,to_active],
                  temp_active_states[i, end_active]), 
                rep(d_thresholds$threshold_a,2), lwd = 3, col = "orange")
          # text(x = mean(data_t$ts[trunc(rowMeans(temp_active_states[["state_mat"]][i,c(1,2)]))]), y = threshold, label = temp_active_states[["state_mat"]][i,3], pos = 3)
        }
        abline(h = d_thresholds$threshold_a, lty = 2, col = "orange")
        
      # plot temp_active_states for threshold_b
        temp_active_states <- predict_state.f(parameter = d_activity[,get(axis)], 
                                         parameter_ma = "no",
                                         ts = d_activity$ts, 
                                         2, d_thresholds$threshold_b)[["active_states"]]
        
        for (i in 1:nrow(temp_active_states)){
          lines(c(temp_active_states[i,to_active],
                  temp_active_states[i, end_active]), 
                rep(d_thresholds$threshold_b,2)+0.5, lwd = 3, col = "purple")
          # text(x = mean(data_t$ts[trunc(rowMeans(active_states[["state_mat"]][i,c(1,2)]))]), y = threshold, label = temp_active_states[["state_mat"]][i,3], pos = 3)
        }
        abline(h = d_thresholds$threshold_b+0.5, lty = 2, col = "purple")
        
        # plot temp_active_states for threshold_c
        temp_active_states <- predict_state.f(parameter = d_activity[,get(axis)], 
                                         parameter_ma = "no",
                                         ts = d_activity$ts, 
                                         2, d_thresholds$threshold_c)[["active_states"]]
        
        for (i in 1:nrow(temp_active_states)){
          lines(c(temp_active_states[i,to_active],
                  temp_active_states[i, end_active]), 
                rep(d_thresholds$threshold_c,2)-0.5, lwd = 3, col = "green")
          # text(x = mean(data_t$ts[trunc(rowMeans(temp_active_states[["state_mat"]][i,c(1,2)]))]), y = threshold, label = temp_active_states[["state_mat"]][i,3], pos = 3)
        }
        abline(h = d_thresholds$threshold_c-0.5, lty = 2, col = "green")}
     },
  axis = picker("act_x", "act_y", "act_xy"),
  d_day = picker(label = "day", as.list(as.character(unique(
    as.Date(activity[,ts]))))),
  plot_gps = checkbox(initial = FALSE),
  plot_movement = checkbox(initial = FALSE),
  plot_moving_window = checkbox(initial = FALSE),
  plot_state  = checkbox(initial = FALSE),
  window_width_activity_ma = slider(min = 1, max = 60, step = 1, initial = 3),
  threshold = slider(min = 0, max = 150, step = 1, initial = 0),
  min_duration_active_state = slider(min = 0, max = 150, step = 5, initial = 5),
  plot_predicted_state  = checkbox(initial = FALSE))
}    