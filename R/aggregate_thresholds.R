#' Aggregate activity thresholds
#'
#' \code{aggregate_thresholds} aggregates the daily activity thresholds (return
#'   of \code{\link{activity2thresholds}} by \code{period}.
#'
#' @param thresholds A data.table with the activity thresholds (return of
#'   \code{\link{activity2thresholds}} with additional column
#'   \code{threshold_period)} by which to aggregate the threshold values.
#'
#' @examples
#' aggregate_tresholds(thresholds = activity_thresholds_final)
#'
#' @import data.table
#' @export

aggregate_thresholds <- function(thresholds) {
  if (!('threshold_period' %in% colnames(thresholds))){
    stop("You have to add a column <threshold_period> by which the thresholds
  will be aggregated.")
  } else {
  thresholds[!is.na(threshold_a),
            .(threshold_a = round(mean(threshold_a)),
              threshold_a_se = round(sd(threshold_a)/sqrt(.N),2),
              threshold_b = round(mean(threshold_b)),
              threshold_b_se = round(sd(threshold_b)/sqrt(.N),2),
              threshold_c = round(mean(threshold_c)),
              threshold_c_se = round(sd(threshold_c)/sqrt(.N),2)),
             by = .(animal_tag,
                    threshold_period)]
    }
}
