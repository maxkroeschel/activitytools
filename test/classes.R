library(devtools)
load_all("~/projects/activitytools")
Sys.setenv(TZ='UTC')

# Test activity class ----
data(activity_data)
data(gps_data)
pars <- list(axis = "act_xy",
             reg.minutes = 5,
             smooth.width_ma = 2,
             tresh.n_runs = 1,
             thresh.window_width_around_day = 3,
             thresh.n_thresholds = c(25:35),
             thresh.min_bin_width = 1,
             thresh.min_duration_active_state = 10)
# simple
deer <- activity(activity_data = activity_data)
# with GPS
deer <- activity(activity_data = activity_data,
                 gps = gps_data)
# with parameters
deer <- activity(activity_data = activity_data,
                 gps = gps_data,
                 parameters = pars)
# based on other activity object
deeragain <- activity(activity_data = deer$activity_data,
                      gps = deer$gps_data,
                      parameters = deer$parameters)
# with only some parameters
deer <- activity(activity_data = activity_data,
                 gps = gps_data,
                 parameters = pars[1:2])
# Don't keep source data
deer <- activity(activity_data = activity_data,
                 gps = gps_data,
                 parameters = pars,
                 keep_source = FALSE)
# Timestamp not POSIX
activity_data_noPOSIX <- activity_data
activity_data_noPOSIX$ts <- as.character(activity_data$ts)
deer <- activity(activity_data = activity_data_noPOSIX)
deer$activity_data$ts

# Throw some errors
deer <- activity(activity_data = NULL,
                 gps = gps_data)
deer <- activity(activity_data = activity_data[,c(1,3)])
deer <- activity(activity_data = activity_data,
                 gps = gps_data[,c(1,4)])


## Workflow 1 - all model parameters first ----
data(activity_data)
data(gps_data)

# list of all parameters
pars <- list(act.axis = "act_xy",
             reg.minutes = 5,
             smooth.width_ma = 2,
             thresh.n_runs = 1,
             thresh.window_width_around_day = 3,
             thresh.n_thresholds = c(25:35),
             thresh.min_bin_width = 1,
             thresh.min_duration_active_state = 10,
             states.pos = NULL,
             states.dayshift = "dawn",
             states.dawn_degree = 12,
             states.period = "day",
             states.max_na = 30)

deer <- activity(activity_data = activity_data, gps_data = gps_data, parameters = pars)

# Regularize
deer$activity_data[,.(min = min(diff(ts)),
                 max = max(diff(ts)),
                 mean = mean(diff(ts))),
              by = animal_tag]

deer <- regularize_activity(deer)
deer$activity_data[,.(min = min(diff(ts)),
                      max = max(diff(ts)),
                      mean = mean(diff(ts))),
                   by = animal_tag]

# Combine activity_data from x- and y-axis: Already performed when creating
# activity obect; updated after regularization

# Smooth activity
deer <- smooth_activity(deer)

# Identify and remove activity gaps
deer <- identify_activity_gaps(deer)
deer <- remove_activity_gaps(deer)

# Thresholds
# deer <- calculate_thresholds(deer, plot_summary = TRUE)
# Resulting 'deer' object:
data("thresholds")

# Activity states (using parameter list defined above)
deer_states <- states(deer, parameters = pars)

# States again with different parameters
deer_states2 <- states(activity = deer,
                       thresholds = c("a", "c"),
                       parameters = list(states.dayshift = "sunrise",
                                         states.dawn_degree = 11,
                                         states.period = "week",
                                         states.max_na = 210))

## Plotting
# Plot thresholds and activity
plot_thresholds(deer)
plot_activity(deer, animal_id = 1)
# These functions also work for 'states' objects if input data is kept
plot_thresholds(deer_states)
plot_activity(deer_states, animal_id = 1)

# Plot active states
plot_states(deer_states, threshold = "b")

# Export classified GPS data
write.table(deer_states$threshold_a$gps_active, file = "./test/gps_data_export.csv",
            sep  = ";", row.names = FALSE)


# same using function arguments
deer_states2 <- states(activity = deer,
                       thresholds = c("a", "c"),
                       states.dayshift = "sunrise",
                       states.dawn_degree = 11,
                       states.period = "week",
                       states.max_na = 210)

deer_states2 <- states(activity = deer,
                       parameters = pars,
                       thresholds = c("a", "c"),
                       states.dayshift = "sunrise",
                       states.dawn_degree = 11,
                       states.period = "week",
                       states.max_na = 210)



## Other
deer$gps_data
deer <- add_gps_data(deer, gps_data)


## Workflow 2 - parameters step by step ----
data(activity_data)
data(gps_data)
deer <- activity(activity_data = activity_data, gps = gps_data)

# Regularize
deer$activity_data[,.(min = min(diff(ts)),
                      max = max(diff(ts)),
                      mean = mean(diff(ts))),
                   by = animal_tag]

deer <- regularize_activity(activity = deer,
                            reg.minutes=5)
deer$activity_data[,.(min = min(diff(ts)),
                      max = max(diff(ts)),
                      mean = mean(diff(ts))),
                   by = animal_tag]

# Combine activity_data from x- and y-axis: Already performed when creating
# activity obect; updated after regularization

# Smooth activity
deer <- smooth_activity(activity = deer,
                        act.axis = 'act_xy',
                        smooth.width_ma = 2,
                        update_NA = TRUE)

# Identify and remove activity gaps
deer <- identify_activity_gaps(deer,
                               act.axis = 'act_xy')
deer <- remove_activity_gaps(deer)

# Thresholds -- includes aggregation
# deer <- calculate_thresholds(activity = deer,
#                               thresh.n_runs = 1,
#                               thresh.window_width_around_day = 3,
#                               thresh.n_thresholds = c(25:35),
#                               thresh.min_bin_width = 1,
#                               thresh.min_duration_active_state = 10,
#                               plot_summary = TRUE)
data("thresholds")

# Activity states
deer_states <- states(deer,
                      thresholds = c("a", "b", "c"),
                      states.dayshift = "dawn",
                      states.dawn_degree = 12,
                      states.period = "day",
                      states.max_na = 30)

# using different parameters
deer_states2 <- states(activity = deer,
                       thresholds = c("a", "c"),
                       states.dayshift = "sunrise",
                       states.dawn_degree = 11,
                       states.period = "week",
                       states.max_na = 210)


# GPS data ----

# GPS data can also be added or changed at later stages

deer <- add_gps_data(deer, gps_data)
deer_states <- add_gps_data(deer_states, gps_data)


# More notes on usage ----

# In creating a states object, the parameter list and the function arguments can
# be mixed. In this case, function arguments always override the parameter list
# (as well as the parameters already stored in the activity or states object).

deer_states2 <- states(activity = deer,
                       thresholds = c("a", "c"),
                       parameters = list(states.dayshift = "dawn",
                                         states.dawn_degree = 12,
                                         states.period = "day",
                                         states.max_na = 30),
                       states.dayshift = "sunrise",
                       states.dawn_degree = 11,
                       states.period = "week",
                       states.max_na = 210)
deer_states2$parameters
deer_states2 <- states(activity = deer,
                       thresholds = c("a", "c"),
                       parameters = list(states.dayshift = "dawn",
                                         states.dawn_degree = 12,
                                         states.period = "day",
                                         states.max_na = 30),
                       states.dayshift = "sunrise",
                       states.dawn_degree = 11,
                       states.period = "week",
                       states.max_na = 210)

# This does not keep input data in the states object:
deer_states3 <- states(activity = deer,
                       thresholds = c("a", "b", "c"),
                       parameters = list(states.dayshift = "dawn",
                                         states.dawn_degree = 12,
                                         states.period = "day",
                                         states.max_na = 30),
                       keep_input = FALSE)

