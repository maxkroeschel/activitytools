#' Activity2thresholds
#'
#' \code{activity2thresholds}
#'
#' @param activity A data.table with the activity data. The following columns
#'   should be present: 'animal_tag' and 'ts'.
#' @param activity_gaps A data.table with the identified gaps in the activity
#'    data (return of function \code{\link{identify_activity_gaps}}.
#' @param act a
#' @param act_ma a
#' @param reg_minutes
#' @param n_runs a
#' @param window_width_around_day a
#' @param n_thresholds a
#' @param resting_range_limit a
#' @param threshold_range_limit a
#' @param min_duration_active_state a
#' @param plot_summary
#'
#' @return Data.table with the ...
#' @examples
#' activity_thresholds <- activity2thresholds(activity = activity_data,
#'                               activity_gaps = activity_data_gaps,
#'                               act = 'act_xy',
#'                               act_ma = 'act_xy_ma2',
#'                               reg_minutes = 5,
#'                               n_runs = 1,
#'                               window_width_around_day = 3,
#'                               n_thresholds = c(25:35),
#'                               resting_range_limit = 0.2,
#'                               threshold_range_limit = 0.8,
#'                               min_duration_active_state = 2,
#'                               plot_summary = TRUE)
#'
#' @import data.table
#' @export


activity2thresholds <- function(activity,
                                activity_gaps,
                                act,
                                act_ma,
                                reg_minutes,
                                n_runs,
                                window_width_around_day,
                                n_thresholds,
                                resting_range_limit,
                                threshold_range_limit,
                                min_duration_active_state,
                                plot_summary) {

# oldpar <- par(no.readonly=TRUE)
# on.exit(par(oldpar))

# Local functions and parameters.
################################################################################

# Define the minimum difference between the candidate thresholds.
  min_bin_width <- 1

# Function to calculate the position of the part of the value range where
# activity data originating from resting periods are expected.
  resting_range_threshold.f <-
    function(x) {(max(x, na.rm=T)-min(x, na.rm=T)) * resting_range_limit +
                  min(x, na.rm=T)}

# Function to calculate the position of the highest maximum of a density
# distribution.
  dmax.f <- function (x) {
    den <- density(x, na.rm=T)
    den <- smooth.spline(den$x, den$y, all.knots=TRUE, spar=0.8)
    max <-   den[["x"]][
      match(sort(den[["y"]][which(diff(sign(diff(den[["y"]])))==-2)],
                 decreasing = TRUE), den[["y"]])]
    return(max)}

# Function to calculate threshold of type a.
  threshold_a.f <- function (x) {
    return(mean(x[x <= quantile(x, probs = 0.33)]) +
             3* sd(x[x <= quantile(x, probs = 0.33)]))}

# Function to calculate threshold of type b.
  threshold_b.f <- function (x) {
    return(mean(x[x <= quantile(x, probs = 0.75)]) +
             3* sd(x[x <= quantile(x, probs = 0.75)]))}

# Function to calculate threshold of type c.
  iqr15_threshold.f <- function (x) {
    lowerq = quantile(x)[2]
    upperq = quantile(x)[4]
    iqr = upperq - lowerq
    upper_threshold = (iqr * 1.5) + upperq
    return(upper_threshold)}

# Function start
################################################################################

do.call("rbind",
 lapply (activity[,unique(animal_tag)], function(x_animal_tag) {
  x_activity <- activity[animal_tag == x_animal_tag,,]
  print(paste("animal_tag:", x_animal_tag, "- processing!", sep = " "))

# Create a sequence of observation days.
  # check if the column day exists
  if(!"day" %in% names(x_activity)) {
      x_activity[,day := as.Date(ts),]
    }
  day_seq <- x_activity[!is.na(get(act)), unique(day), ]
  day_seq <- sort(day_seq)

# Calculate threshold values for each observation day.
return(
  do.call("rbind",
          lapply(1:n_runs, function(x_run) {
            print(paste("run: ", x_run))
            return(

  do.call("rbind",
          lapply(day_seq, function(i_day) {
      temp_warning <- NA

# Select a temporary data set of day i.
  i_activity <-
    x_activity[day >= as.Date(i_day) - lubridate::days(window_width_around_day) &
               day <= as.Date(i_day) + lubridate::days(window_width_around_day ),]

# check i) if the data set contains not enough data points (enough means data should at
#             least cover 20 hours) and
#      ii) if all data are equal to 0
# --> end currend loop and return an empty data.frame with a warning message
  if ((i_activity[!is.na(get(act_ma)),.N,] < 1200/reg_minutes) |
       (sum(i_activity[!is.na(get(act_ma)),get(act_ma),]) == 0)) {
         return(data.table("animal_tag" = x_animal_tag,
                           "day" = i_day,
                           "threshold_a" = NA,
                           "threshold_b" = NA,
                           "threshold_c" = NA,
                           "n_thresholds" = NA,
                           "bin_width" = NA,
                           "run" = NA,
                           "warning" = "There are not enough activity data for this period or all activity data are 0!"))
     } else {

# Create candidate thresholds.

# Set the number of splits.
 if (length(n_thresholds) == 1) {temp_n_thresholds <- n_thresholds} else {
     temp_n_thresholds <- sample(n_thresholds, size = 1)}

# Set the first candidate threshold to the position of the highest maximum of
# the resting range.
  resting_range_threshold <-
    resting_range_threshold.f(i_activity[,get(act_ma),])
  min_seq <-
    max(1, round(dmax.f(i_activity[get(act_ma) <=  resting_range_threshold,
                                    get(act_ma),])[1]))

# Set the last candidate threshold matching the threshold_range_limit.
  max_seq <-
    max(3,quantile(i_activity[, get(act_ma)],
                   probs = (threshold_range_limit), na.rm =T))

# Calculate the increment of the candidate thresholds.
  temp_bin_width <- max(min_bin_width, round((max_seq - min_seq)/temp_n_thresholds))

# Adjust the number of candidate thresholds.
  temp_n_thresholds <- max(10, round((max_seq - min_seq)/temp_bin_width))

# Calculate the final candidate thresholds.
  threshold_seq <- seq(from = min_seq,
                       length = temp_n_thresholds,
                       by = temp_bin_width)

# Calculate active states for each candidate threshold.
  ls <- lapply(threshold_seq, FUN = function(i_threshold_seq){
        activity2states(activity = i_activity,
                        activity_gaps = activity_gaps,
                        act = act,
                        act_ma = act_ma,
                        threshold = i_threshold_seq,
                        min_duration_active_state = min_duration_active_state)
        })
  names(ls) <- threshold_seq

# Calculate the PTA for each candidate threshold.
  prop_state_active <-
    data.table(threshold = threshold_seq,
               prop_state_active =
                 sapply(ls, function(x) sum(x[["duration"]])) /
                                          as.integer(difftime(max(i_activity[, ts]), min(i_activity[, ts]), units = "mins"))
               )

# Calculate dPTA for each candidate threshold.
  prop_state_active$firstdiff <-
    c(NA, abs(diff(prop_state_active$prop_state_active)))

  # Add values for the first split
    prop_state_active[1,"firstdiff"] <-
      1- prop_state_active[1,"prop_state_active"]

# Select threshold of type a.
  a <- threshold_a.f(prop_state_active$firstdiff)
  threshold_a <- ceiling(threshold_seq[min(which(prop_state_active$firstdiff <= a))])
  # Check if threshold_a could be found.
    if (length(threshold_a) == 0) {
      threshold_a <- NA
      if (is.na(temp_warning)) {
        temp_warning <- "Threshold of type a could not be found!"
        } else {
        temp_warning <- paste(temp_warning, "Threshold of type a could not be found!", sep = " ")
        }
    }

# Select threshold of type b.
  b <- threshold_b.f(prop_state_active$firstdiff)
  threshold_b <- ceiling(threshold_seq[ min(which(prop_state_active$firstdiff <= b))])
  # Check if threshold_b could be found.
    if (length(threshold_b) == 0) {
      threshold_b <- NA
      if (is.na(temp_warning)) {
        temp_warning <- "Threshold of type b could not be found!"
        } else {
        temp_warning <- paste(temp_warning, "Threshold of type b could not be found!", sep = " ")
        }
    }

# Select threshold of type c.
  c <- iqr15_threshold.f(prop_state_active$firstdiff)
  threshold_c  <- ceiling(threshold_seq[min(which(prop_state_active$firstdiff <= c))])
  # Check if threshold_a could be found.
  if (length(threshold_c) == 0) {
    threshold_c <- NA
    if (is.na(temp_warning)) {
    temp_warning <- "Threshold of type c could not be found!"
    } else {
    temp_warning <- paste(temp_warning, "Threshold of type c could not be found!", sep = " ")
    }
  }

# Plot evaluation plots.
################################################################################
if (plot_summary == TRUE){
  layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
  par(mar = c(4.1,4.1,5.1,2.1))
  plot(density(i_activity[,get(act_ma)], na.rm=T),
       xlab = "Activity (smoothed)",
       main = paste("Animal_tag: ", x_animal_tag, "  -  Day: ", i_day, sep = ""),
       lwd = 2,
       xlim = c(-10, max_seq + max_seq*0.25))
  abline(v=threshold_seq, col = "black")
  abline(v=threshold_a, col = "orange", lty = 2)
  abline(v=threshold_b, col = "purple", lty = 2)
  abline(v=threshold_c, col = "cyan2", lty = 2)
  abline(h = 0)
# text(x = max_seq + max_seq*0.035, y = 0, adj = c(0,-1),
#      labels = paste('n_thresholds   = ', temp_n_thresholds,
#                     '\nbin_width = ', temp_bin_width,
#                     '\nwarnings =  ', temp_warning, sep = ""), col = "black")

  par(mar = c(6.1,4.1,3.1,2.1))
  plot(prop_state_active$threshold, prop_state_active$firstdiff,
       type = "h",
       xlab = expression(paste("Thresholds (", theta,")")), # expression('thresholds (t'['i']*')'),
       ylab = "dPTA",
       xlim = c(-10, max_seq + max_seq*0.25),
       ylim = c(0,sort(prop_state_active$firstdiff, decreasing = TRUE)[2] +
                  sort(prop_state_active$firstdiff, decreasing = TRUE)[2]*0.3))
    points(threshold_a, a, col  = "orange")
    points(threshold_b, b, col  = "purple")
    points(threshold_c, c, col  = "cyan2")
    abline(h = a, col = "orange", lty = 2)
    abline(h = b, col = "purple", lty = 2)
    abline(h = c, col = "cyan2", lty = 2)
  par(mar = c(5.1,4.1,4.1,2.1), mfrow = c(1,1))
}

  return(data.table("animal_tag" = as.character(x_animal_tag),
                    "day" = i_day,
                    "threshold_a" = threshold_a,
                    "threshold_b" = threshold_b,
                    "threshold_c" = threshold_c,
                    "n_thresholds" = temp_n_thresholds,
                    "bin_width" = temp_bin_width,
                    "run" = x_run,
                    "warning" = as.character(temp_warning)))
    }
  })))
}))
)# end of each run
}))
}
