# test-script for activity tools
################################################################################

Sys.setenv(TZ='UTC')

# library("RPostgreSQL")

# con_animal_telemetry_db = dbConnect(dbDriver("PostgreSQL"), user="max",
#                                     password="", host="localhost",
#                                     port=54321, dbname="animal_telemetry")
#
#
# activity_data <- dbGetQuery(con_animal_telemetry_db,
#                             "select
#                                 animal_id,
#                                 tag_code,
#                                 act_x,
#                                 act_y,
#                                 ts
#                               from
#                                 core.activity_data_animals
#                               where
#                                 animal_id in (64,67);")
#
# gps_data <- dbGetQuery(con_animal_telemetry_db,
#                             "select
#                                 animal_id,
#                                 tag_code,
#                                 longitude,
#                                 latitude,
#                                 dop,
#                                 ts
#                               from
#                                 core.gps_points_animals
#                               where
#                                 animal_id in (64,67);")

# save(activity_data, gps_data, file = "./test/test_data.RData")


load("test/test_data.RData")

activity_data <- data.table(activity_data)[order(animal_id, ts),,]
gps_data <- data.table(gps_data)[order(animal_id, ts),,]

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
                                 axis = 'act_xy',
                                 width_axis_ma = 2,
                                 update_NA = TRUE)

# identify and store data gaps in the activity data
activity_data_gaps <- identify_activity_gaps(activity = activity_data,
                                             axis = 'act_xy')

# remove data gaps
activity_data <- remove_activity_gaps(activity = activity_data,
                                      activity_gaps = activity_data_gaps)

# restore columns animal_id and tag_code
activity_data <- split_animaltag(data = activity_data)

# calculate threshold values (this takes some time)
activity_thresholds <-
  data.table(activity2thresholds(activity = activity_data,
                                 activity_gaps = activity_data_gaps,
                                 axis = 'act_xy',
                                 axis_ma = 'act_xy_ma2',
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
