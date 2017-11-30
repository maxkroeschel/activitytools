#' Create identifer for the week of the year from timestamp.
#'
#' \code{ts2yearweek}
#'
#' @param ts
#' @return
#' @examples
#'
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
