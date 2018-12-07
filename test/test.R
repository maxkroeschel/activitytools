#
################################################################################

Sys.setenv(TZ='UTC')

#library("activitytools")

library(devtools)
load_all("./")

data(activity_data)
data(gps_data)

# create parameter that is used for state classification
activity_data$act_xy <- activity_data$act_x + activity_data$act_y

# define all parameter that are used for state detection

pars <- list(act.act = "act_xy",
             act.reg_minutes = 5,
             act.smooth_width_ma = 2,
             thresh.n_runs = 1,
             thresh.window_width_around_day = 3,
             thresh.n_thresholds = c(25:35),
             thresh.min_bin_width = 1,
             states.min_duration_active = 10,
             pta.pos = NULL,
             pta.dayshift = "dawn",
             pta.dawn_degree = 12,
             pta.period = "day",
             pta.max_na = 30)

# create activity object
  ## parameter can also be defined later
deer <- activity(activity_data = activity_data, gps_data = gps_data, parameters = pars)

# activity data are often characterized by data gaps or duplicated data points
  # deer$activity_data[,.(min = min(diff(ts)),
  #                       max = max(diff(ts)),
  #                       mean = mean(diff(ts))),
  #                    by = animal_tag]

# --> create a data set with equal time intervals between the activity data (regularize)
#     the regular time interal is defined by the parameter 'act.reg_minutes'

deer <- regularize_activity(deer)

  # deer$activity_data[,.(min = min(diff(ts)),
  #                       max = max(diff(ts)),
  #                       mean = mean(diff(ts))),
  #                    by = animal_tag]


# Smooth the activity axis with a moving average
deer <- smooth_activity(deer)

# Identify gaps in the activity data
deer <- identify_activity_gaps(deer)
(deer$activity_gaps)

# Remove gaps in the activity data
deer <- remove_activity_gaps(deer)

# Calculate thresholds (this might take some time !!!!!)
deer <- calculate_thresholds(deer, plot_summary = TRUE)

# Calculate states, the proportion of time in state active and assign each
# GPS-position as active or resting (using parameter list defined above)
deer <- calculate_states(deer)

# plotting options
# Thresholds
plot(deer, select = "thresholds")
# Activity data
plot(deer, animal_id = 1)
plot(deer, select = "activity", animal_id = 1) # same
# Active states
plot(deer, select="states", threshold = "a")
