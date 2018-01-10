#' Convert timestamp to year_week.
#'
#' \code{ts2yearweek} extracts the week of the year and the year from a
#'   timestamp and concatenates both to create a unique identifier for each week
#'   of the year. Around the turn of the year, sometimes, the week of the year
#'   switches already in the last days of the year, or does not switch at the
#'   first days of the new year. These days are corrected to maintain continuous
#'   numbering.
#'
#' @param ts A vector with timestamps or dates in format POSIXct or Date.
#' @return A vector with year_week identifier for each week of the year.
#' @examples
#'  ts2yearweek(as.Date("2017-01-01"))
#' @export

ts2yearweek <- function(ts) {
  year <- strftime(ts, format = "%Y")
  week <- strftime(ts, format = "%V")

  # correct for the last days of the year that are assigned to the first week of
  # the next year
  error1 <- which(strftime(ts, format = "%m") == "12" &
                    strftime(ts, format = "%V") == "01")
  if (length(error1) > 0) {
    year[error1] <- as.character(
                      as.numeric(strftime(ts[error1], format = "%Y")) + 1
                      )
  }

  # correct for the first days of the year that are assigned to the last week of
  # the previous year
  error2 <- which(strftime(ts, format = "%m") == "01" &
                    strftime(ts, format = "%V") %in% c("52","53"))
  if (length(error2) > 0) {
    year[error2] <- as.character(
                      as.numeric(strftime(ts[error2], format = "%Y")) - 1
                    )
  }
  return(paste(year, week,sep = "-"))
}
