# test-script for activity tools
################################################################################

Sys.setenv(TZ='UTC')
library("activitytools")

data(activity_data)
data(gps_data)

activity_data <- data.table(activity_data)[order(animal_id, ts),,]
gps_data <- data.table(gps_data)[order(animal_id, ts),,]
gps_data <- create_animaltag(gps_data)

# activity_data <- activity_data[as.Date(ts) >= as.Date('2007-06-01') &
#                                  as.Date(ts) < as.Date('2007-07-01'),, ]

activity_data <- create_animaltag(activity_data)

# regularize the activity data
activity_data[,.(min = min(diff(ts)),
                 max = max(diff(ts)),
                 mean = mean(diff(ts))),
              by = animal_tag]

activity_data <- regularize_activity(activity = activity_data,
                                     minutes = 5)

activity_data[,.(min = min(diff(ts)),
                 max = max(diff(ts)),
                 mean = mean(diff(ts))),
              by = animal_tag]

# combine activity_data from x- and y-axis
activity_data[, act_xy := act_x + act_y]

activity_data <- smooth_activity(activity = activity_data,
                                 act = 'act_xy',
                                 width_act_ma = 2,
                                 update_NA = TRUE)

# identify and store data gaps in the activity data
activity_data_gaps <- identify_activity_gaps(activity = activity_data,
                                             act = 'act_xy')

# remove data gaps
activity_data <- remove_activity_gaps(activity = activity_data,
                                      activity_gaps = activity_data_gaps)

# restore columns animal_id and tag_code
activity_data <- split_animaltag(data = activity_data)

# calculate threshold values (this takes some time)
activity_thresholds <-
  data.table(activity2thresholds(activity = activity_data,
                                 activity_gaps = activity_data_gaps,
                                 act = 'act_xy',
                                 act_ma = 'act_xy_ma2',
                                 n_runs = 1,
                                 window_width_around_day = 3,
                                 n_thresholds = c(25:35),
                                 min_bin_width = 1,
                                 min_duration_active_state = 10,
                                 plot_summary = TRUE))

# create period_id inside the tables activity_thresholds and activity_data
activity_thresholds[, threshold_period := ts2yearweek(day),]
activity_data[,threshold_period := ts2yearweek(ts),]

# aggregate activity thresholds
activity_thresholds_final <-
  aggregate_thresholds(thresholds = activity_thresholds)

activity_thresholds_final <- split_animaltag(activity_thresholds_final)

plot_thresholds(thresholds = activity_thresholds_final)

plot_activity(activity = activity_data,
              animal_id = "64",
              gps = gps_data,
              thresholds = activity_thresholds_final)


# predict active states for each animal
active_states_a <-  thresholds2states(activity = activity_data,
                                      activity_gaps = activity_data_gaps,
                                      thresholds = activity_thresholds_final,
                                      threshold_par = 'threshold_a')

active_states_b <-  thresholds2states(activity = activity_data,
                                      activity_gaps = activity_data_gaps,
                                      thresholds = activity_thresholds_final,
                                      threshold_par = 'threshold_b')

prop_time_active <- states2prop_time_active(active_states = active_states_a,
                                            activity_gaps = activity_data_gaps,
                                            gps = gps_data,
                                            dayshift = "dawn",
                                            dawn_degree = 12,
                                            max_na_per_day = 30)

plot_states(active_states = active_states_a,
            prop_time_active = prop_time_active_a,
            gps = gps_data)

gps_data <- states2gps(gps = gps_data,
                        active_states = active_states_a)
gps_data <- daytime2gps(gps = tgps_data,
                         dawn_degree = 12)
gps_data[,date := as.Date(ts),]
gps_data <- split_animaltag(gps_data)
write.table(gps_data, file = "./test/gps_data_export.csv",
          sep  = ";", row.names = FALSE)

gps_data[, daytime := daytime2gps(long = longitude,
                        lat = latitude,
                        ts = ts,
                        dawn_degree = 12,
                        type = "daytime")]
