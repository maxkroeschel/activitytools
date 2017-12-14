#' Timestamp 2 minutes
#'
#' \code{ts2minutes} calculates the elapsed minutes of the day from 00:00.
#'
#' @param ts A vector of timestamps with class POSIXct.
#'
#' @return A integer vector with the elapsed minutes since 00:00.
#'
#' @examples
#'
#'
#' @import data.table

ts2minutes <- function(x) {
  ceiling(as.numeric(as.difftime(strftime(x, format = "%H:%M:%S", tz = "UTC"),
                                 format = "%H:%M:%S", units = "mins")))
  }
