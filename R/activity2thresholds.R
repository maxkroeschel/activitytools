#' Activity2thresholds
#'
#' \code{activity2thresholds}
#'
#' @param activity A data.table with the activity data. The following columns
#'   should be present: 'animal_tag' and 'ts'.
#' @param activity_gaps A data.table with the identified gaps in the activity
#'    data (return of function \code{\link{identify_activity_gaps}}.
#' @param axis a
#' @param axis_ma a
#' @param n_runs a
#' @param window_width_around_day a
#' @param n_thresholds a
#' @param min_bin_width a
#' @param min_duration_active_state a
#' @param plot summary
#'
#' @return Data.table with the ...
#' @examples
#' activity_thresholds <- activity2thresholds(activity = activity_data,
#'                               activity_gaps = activity_data_gaps,
#'                               axis = 'act_xy',
#'                               axis_ma = 'act_xy_ma2',
#'                               n_runs = 1,
#'                               window_width_around_day = 3,
#'                               n_thresholds = c(25:35),
#'                               min_bin_width = 1,
#'                               min_duration_active_state = 10,
#'                               plot_summary = TRUE)
#'
#' @import data.table
#' @export

# source("/home/max/Dropbox/work/activitytools/R/activity2states.R")
# rename to activity2state

# (1) predict state for a sequence of threshold values
  activity2thresholds <- function(activity,
                                  activity_gaps,
                                  axis,
                                  axis_ma,
                                  n_runs,
                                  window_width_around_day,
                                  n_thresholds,
                                  min_bin_width,
                                  min_duration_active_state,
                                  plot_summary) {

oldpar <- par(no.readonly=TRUE)
on.exit(par(oldpar))

#  local functions

# calculate the position of the highest maximum of a density distribution
# --> used to calculate the start of the threshold_seq
  dmax.f <- function (x) {
    den <- density(x, na.rm=T)
    den <- smooth.spline(den$x, den$y, all.knots=TRUE, spar=0.8)
    max <-   den[["x"]][
      match(sort(den[["y"]][which(diff(sign(diff(den[["y"]])))==-2)],
                 decreasing = TRUE), den[["y"]])]
    return(max)}

# function to calculate threshold_a
  threshold_a.f <- function (x) {
    return(mean(x[x <= quantile(x, probs = 0.33)]) +
             3* sd(x[x <= quantile(x, probs = 0.33)]))}

# function to calculate threshold_b
  threshold_b.f <- function (x) {
    return(mean(x[x <= quantile(x, probs = 0.75)]) +
             3* sd(x[x <= quantile(x, probs = 0.75)]))}

# function to calculate threshold_c
  iqr15_threshold.f <- function (x) {
    lowerq = quantile(x)[2]
    upperq = quantile(x)[4]
    iqr = upperq - lowerq
    upper_threshold = (iqr * 1.5) + upperq
    return(upper_threshold)}

do.call("rbind",
 lapply (activity[,unique(animal_tag)], function(x_animal_tag) {
  x_activity <- activity[animal_tag == x_animal_tag,,]
  print(paste("animal_tag:", x_animal_tag, "- processing!", sep = " "))

  # create a sequence of observation days
  # check if the column day exists
  if(!"day" %in% names(x_activity)) {
    x_activity[,day := as.Date(ts),]
  }
  day_seq <- x_activity[!is.na(get(axis)), unique(day)]
  day_seq <- sort(day_seq)

# calculate threshold values for each observation day
#  interval: [observation day - window width, observation day + window width]
  return(
    do.call("rbind",
            lapply(1:n_runs, function(x_run) {
              print(paste("run: ", x_run))
              return(

    do.call("rbind",
            lapply(day_seq, function(i_day) {
        # print (i_day)
        temp_warning <- NA

# select the temporary data set of the specific day
        i_activity <- x_activity[day >= as.Date(i_day) - lubridate::days(window_width_around_day) &
                                        day <= as.Date(i_day) + lubridate::days(window_width_around_day ),]

  # check if 1) the data set contains enough data points (enough means >288) and
  #          2) if all data are equal to 0
  # --> end currend loop and return an empty data.frame with a warning message
        if ((i_activity[!is.na(get(axis_ma)),.N,] < 288) |
             (sum(i_activity[!is.na(get(axis_ma)),get(axis_ma),]) == 0)) {
               return(data.table("animal_tag" = x_animal_tag,
                                 "day" = i_day,
                                 "threshold_a" = NA,
                                 "threshold_b" = NA,
                                 "threshold_c" = NA,
                                 "n_thresholds" = NA,
                                 "bin_width" = NA,
                                 "run" = NA,
                                 "warning" = "There are not enough activity data for this period or all activity data are 0!",
                                 "axis" = axis,
                                 "axis_ma" = axis_ma,
                                 "min_duration_active_state" = min_duration_active_state,
                                 "window_width_around_day" = window_width_around_day))
             } else {

# calculate the threshold sequence

  # set the number of splits
   if (length(n_thresholds) == 1) {temp_n_thresholds <- n_thresholds} else {
       temp_n_thresholds <- sample(n_thresholds, size = 1)}

  # set the start value of the threshold sequence to the position of the first maximun
  # to facilitate automated selection of the best threshold value
    min_seq <- max(1, round(dmax.f(i_activity[,get(axis_ma),])[1]))

  # sometimes the density distribution is not characterized by a distinct peak
  # at the beginning, that causes a wrong starting position of the threshold
  # sequence --> when the start value of the threshold sequence is larger than
  # the 35quantile then explicitly search for a peak in the first part of the
  # distribution
    if (min_seq != 1 &
        min_seq > i_activity[, quantile(get(axis_ma), na.rm=T, probs = 0.35),]) {
          min_seq <- max(1, round(min(dmax.f(
                                i_activity[get(axis_ma) <= quantile(get(axis_ma), na.rm=T, probs = 0.5),
                                                  get(axis_ma)])),1))
          temp_warning <- 'Density distribution of your axis_ma has no peak before quantile(0.33)!'}

  # set the end value of the threshold sequence
    max_seq <- max(3,quantile(i_activity[, get(axis_ma)], probs = 0.80, na.rm =T))

  # calculate the increment of the threshold sequence
    temp_bin_width <- max(min_bin_width, round((max_seq - min_seq)/temp_n_thresholds))

  # calculate the final threshold sequence
    threshold_seq <- seq(from = min_seq,
                             to = max_seq,
                             length = temp_n_thresholds)
                            # by = temp_bin_width) # + runif(n = 1, min = 0, max = temp_bin_width)

  # store the length of the threshold sequence
    temp_n_thresholds <- length(threshold_seq)

# calculate active states for each threshold value
    ls <- lapply(threshold_seq, FUN = function(i_threshold_seq)
        activity2states(activity = i_activity,
                                  activity_gaps = activity_gaps,
                                  axis = axis,
                                  axis_ma = axis_ma,
                                  #ts,
                                  #width_axis_ma = 2,
                                  threshold = i_threshold_seq,
                                  min_duration_active_state = min_duration_active_state))
        names(ls) <- threshold_seq


        # (2) calculate the proportion of time in state active for the sequence of
        #     threshold values
        prop_state_active <- data.table(
          threshold = threshold_seq,
          prop_state_active = sapply(ls, function(x) sum(x[["duration"]]))    /
            as.integer(difftime(max(i_activity[, ts]),
                                min(i_activity[, ts]),
                                units = "mins")))

        # (3) Algorithm that automatically selects the "best" threshold value

        # calculate the first difference
        prop_state_active$firstdiff <-
          c(NA, abs(diff(prop_state_active$prop_state_active)))

        # add values for the first split
        prop_state_active[1,"firstdiff"] <-
          1- prop_state_active[1,"prop_state_active"]

      # threshold_a = user defined threshold
        # a <- 0.05
        # threshold_a  <- threshold_seq[min(which(prop_state_active$firstdiff < a))-1]
        a <- threshold_a.f(prop_state_active$firstdiff)
        threshold_a <- ceiling(max(1,threshold_seq[ min(which(prop_state_active$firstdiff <= a))-1]))

      # threshold_b = user defined threshold
        b <- threshold_b.f(prop_state_active$firstdiff)
        threshold_b <- ceiling(max(1,threshold_seq[ min(which(prop_state_active$firstdiff <= b))-1]))

      # threshold_c
        c <- iqr15_threshold.f(prop_state_active$firstdiff)
        threshold_c  <- ceiling(max(1,threshold_seq[min(which(prop_state_active$firstdiff <= c))-1]))

# plot evaluation plots

        if (plot_summary == TRUE){

          layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))

          plot(density(i_activity[,get(axis_ma)], na.rm=T),
               xlab = axis_ma,
               main = paste("animal_tag: ", x_animal_tag, "  -  day: ", i_day, sep = ""),
               lwd = 2, xlim = c(-50,500))
          abline(v=threshold_seq, col = "black")
          abline(v=threshold_a, col = "blue", lty = 2)
          abline(v=threshold_b, col = "purple", lty = 2)
          abline(v=threshold_c, col = "green", lty = 2)
#          abline(v=thresholtemp_d, col = "red", lty = 2)
          abline(h = 0)
          text(x = 350, y = 0, adj = c(0,-1),
               labels = paste('n_thresholds   = ', temp_n_thresholds,
                              '\nbin_width = ', temp_bin_width,
                              '\nwarnings =  ', temp_warning, sep = ""))

          plot(prop_state_active$threshold, prop_state_active$firstdiff,
               type = "h", xlab = "threshold", ylab = "first_diff",
               xlim = c(0,150),ylim = c(0,0.3))
          points(threshold_a, a, col  = "blue")
          points(threshold_b, b, col  = "purple")
          points(threshold_c, c, col  = "green")
          abline(h = a, col = "blue", lty = 2)
          abline(h = b, col = "purple", lty = 2)
          abline(h = c, col = "green", lty = 2)

          if (length(prop_state_active[firstdiff < quantile(firstdiff)[4],firstdiff,]) >0){
            plot(density(prop_state_active[firstdiff < quantile(firstdiff)[4],firstdiff,]),
               xlim = range(prop_state_active$firstdiff), main = "")} else {
              plot(density(prop_state_active[,firstdiff,]),
                       xlim = range(prop_state_active$firstdiff), main = "") }
          rug(prop_state_active$firstdiff, lwd = 2)
          abline(v = a, col = "blue", lty= 2)
          abline(v = b, col = "purple", lty= 2)
          abline(v = c, col = "green", lty= 2)
          curve(dnorm(x, mean=mean(prop_state_active[firstdiff < quantile(firstdiff)[4],firstdiff]),
                      sd=sd(prop_state_active[firstdiff < quantile(firstdiff)[4],firstdiff])), col="grey", lwd=2, add=T)
          # lines(density(prop_state_active[firstdiff < quantile(firstdiff)[4],firstdiff]))
        # par(mfrow = c(1,1))
          }

        return(data.table("animal_tag" = as.character(x_animal_tag),
                          "day" = i_day,
                          threshold_a,
                          threshold_b,
                          threshold_c,
                          "n_thresholds" = temp_n_thresholds,
                          "bin_width" = temp_bin_width,
                          "run" = x_run,
                          "warning" = as.character(temp_warning),
                          "axis" = axis,
                          "axis_ma" = axis_ma,
                          "min_duration_active_state" = min_duration_active_state,
                          "window_width_around_day" = window_width_around_day))
        }
  })))
}))
)# end of each run
}))
}

#    return(do.call("rbind", threshold_values))
#  })
#}



