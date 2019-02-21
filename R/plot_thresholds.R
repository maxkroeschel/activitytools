#' Plot thresholds
#'
#' \code{plot_thresholds} plots the calculated activity thresholds against the
#'  period by which thersholds are aggregated.
#'
#' @param thresholds
#'
#' @examples
#' plot_tresholds(thresholds = activity_thresholds_final,
#'                period = 'year_week')
#' @import data.table
#' @export


plot_thresholds <- function(thresholds){

manipulate::manipulate({
  thresholds[, threshold_period := factor(threshold_period),]
  y_max <- thresholds[animal_tag == d_animal_tag ,
                      max(threshold_a + 2*threshold_a_se, na.rm = T) * 1.05]
  plot(thresholds[animal_tag == d_animal_tag, as.numeric(threshold_period) - 0.25],
       thresholds[animal_tag == d_animal_tag, threshold_a],
       ylim = c(0, y_max), col = "orange", pch = 19,
       cex = 0.7, cex.axes = 0.8, axes = F, xlab = "", ylab = "Thresholds",
       main = paste("Thresholds of animal_tag:   ",
                    d_animal_tag, sep = ""))
  axis(2, cex.axes = 0.8)

  xaxis_labels_range <- thresholds[animal_tag == d_animal_tag,
                                            .(min(as.numeric(threshold_period)),
                                              max(as.numeric(threshold_period)))]
  if ( (xaxis_labels_range$V2 - xaxis_labels_range$V1) >10) {
    d_xaxis_labels <- round(seq(from = xaxis_labels_range$V1,
                                to = xaxis_labels_range$V2 , length.out = 10))
  } else {d_xaxis_labels <- xaxis_labels_range$V1 : xaxis_labels_range$V2}
  axis(1, at = d_xaxis_labels,
       labels = levels(thresholds[,threshold_period,])[d_xaxis_labels],
       las = 2, cex.axis = 0.7 )
  abline(h=seq(0, y_max, by = 5), lty = 2, col = "darkgrey")

  # add threshold_b
  points(thresholds[animal_tag == d_animal_tag, as.numeric(threshold_period) - 0.15],
         thresholds[animal_tag == d_animal_tag, threshold_b],
         col = "purple", cex = 0.5, pch = 19)

  # add threshold_c
  points(thresholds[animal_tag == d_animal_tag, as.numeric(threshold_period) + 0.15],
         thresholds[animal_tag == d_animal_tag, threshold_c],
         col = "green", cex = 0.5, pch = 19)

# add confidence intervals
  if (add_ci == TRUE) {
    arrows(thresholds[animal_tag == d_animal_tag, as.numeric(threshold_period) - 0.25],
           thresholds[animal_tag == d_animal_tag, threshold_a - 2* threshold_a_se],
           thresholds[animal_tag == d_animal_tag, as.numeric(threshold_period) - 0.25],
           thresholds[animal_tag == d_animal_tag, threshold_a + 2* threshold_a_se],
           col = "orange", code=3, angle=90, length=0.02)
    arrows(thresholds[animal_tag == d_animal_tag, as.numeric(threshold_period) - 0.15],
           thresholds[animal_tag == d_animal_tag, threshold_b - 2* threshold_b_se],
           thresholds[animal_tag == d_animal_tag, as.numeric(threshold_period) - 0.15],
           thresholds[animal_tag == d_animal_tag, threshold_b + 2* threshold_b_se],
           col = "purple", code=3, angle=90, length=0.02)
    arrows(thresholds[animal_tag == d_animal_tag, as.numeric(threshold_period) + 0.15],
           thresholds[animal_tag == d_animal_tag, threshold_c - 2* threshold_c_se],
           thresholds[animal_tag == d_animal_tag, as.numeric(threshold_period) + 0.15],
           thresholds[animal_tag == d_animal_tag, threshold_c + 2* threshold_c_se],
           col = "green", code=3, angle=90, length=0.02)
  }

},
d_animal_tag = manipulate::picker(label = "animal_tag",
                      as.list(as.character(thresholds[,unique(animal_tag)]))),
add_ci = manipulate::checkbox(label = "add ci " ,initial = FALSE))
}
