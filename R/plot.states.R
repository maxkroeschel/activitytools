#' Plotting method for \code{states} objects
#'
#' \code{plot.states} creates plots for activity object.
#'
#' @param x An object of class \code{states}
#' @param thresholds Type of thresholds. Must be on of \code{"a"}, \code{"b"}, \code{"c"}.
#' @return
#' @examples
#' @import data.table

plot.states <- function(x,
                        select = "states",
                        ...){
  if(!select %in% c("states", "activity", "thresholds")){
    stop("The 'select' argument must be either 'states', 'activity' or 'thresholds'.")
  }
  if(select == "activity"){
    plot_activity(x, ...)
  }
  if(select == "thresholds"){
    plot_thresholds(x, ...)
  }
  if(select == "states"){
    plot_states(x, ...)
  }
}
