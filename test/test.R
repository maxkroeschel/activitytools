#
################################################################################

Sys.setenv(TZ='UTC')

#library("activitytools")

library(devtools)
load_all("./")

data(activity_data)
data(gps_data)

# Check if the timestamps in the activity data and the gps data are specified correctly. All timestamps have to be in 'UTC'.
attr(activity_data$ts, "tzone")
attr(activity_data$ts, "tzone")<- 'UTC'
attr(gps_data$ts, "tzone")
attr(gps_data$ts, "tzone")<- 'UTC'

# create parameter that is used for state classification
activity_data$act_xy <- activity_data$act_x + activity_data$act_y

# define all parameter that are used for state detection

pars <- list(act.act = "act_xy",
             act.reg_minutes = 5,
             act.width_ma = 2,
             # thresh.n_runs = 1,
             # thresh.window_width_around_day = 3,
             # thresh.n_thresholds = c(25:35),
             # thresh.resting_range_limit = 0.2,
             # thresh.threshold_range_limit = 0.8,
             states.min_duration_active = 2,
             pta.pos = NULL,
             pta.dayshift = "dawn",
             pta.dawn_degree = 12,
             pta.period = "day",
             pta.max_na = 30)

# create activity object
  ## parameter can also be defined later
act_red <-
  activity(activity_data = activity_data,
           gps_data = gps_data,
           parameters = pars,
           keep_source = FALSE)

# activity data are often characterized by data gaps or duplicated data points
  # deer$activity_data[,.(min = min(diff(ts)),
  #                       max = max(diff(ts)),
  #                       mean = mean(diff(ts))),
  #                    by = animal_tag]

# --> create a data set with equal time intervals between the activity data (regularize)
#     the regular time interal is defined by the parameter 'act.reg_minutes'

act_red <- regularize_activity(act_red)

  # deer$activity_data[,.(min = min(diff(ts)),
  #                       max = max(diff(ts)),
  #                       mean = mean(diff(ts))),
  #                    by = animal_tag]


# Smooth the activity axis with a moving average
act_red <- smooth_activity(act_red)

# Identify gaps in the activity data
#deer <- identify_activity_gaps(deer)
#(deer$activity_gaps)

# Remove gaps in the activity data
act_red <- remove_activity_gaps(act_red)

# View the activity data and test thresholds
plot(act_red, select = "activity_eval", select_animal_id = 1)
# Calculate thresholds (this might take some time !!!!!)
act_red <- calculate_thresholds(act_red, plot_summary = FALSE)

plot(act_red, select = "thresholds")

# Calculate states, the proportion of time in state active and assign each
# GPS-position as active or resting (using parameter list defined above)

act_red <- calculate_states(act_red, thresholds = c("a", "b", "c"),
                            add = c("pta", "gps"))

# plotting options
# Activity data
plot(act_red, select = "activity", select_animal_id = 1)

# Active states
plot(act_red, select = "states", threshold = c('b'))

