# --> to be done

plot_prop_time_active <- function(prop_time_active) {
manipulate({
  # define start and end of plot
  x_start <- floor_date(min(d_prop_active[animal_id == d_animal_id, date_dawn,]),
                        "month")
  x_end <- ceiling_date(max(d_prop_active[animal_id == d_animal_id, date_dawn,]),
                        "month")
  if (d_smooth == FALSE) {
    plot(d_prop_active[animal_id == d_animal_id, date_dawn,],
         d_prop_active[animal_id == d_animal_id, total,],
         type = "l", ylim = c(0,1), xlab = "Time", ylab = "PTA",
         axes = FALSE, xlim = c(x_start, x_end))
    abline(v=seq(x_start, x_end, by = "month"), lty = 2)
    lines(d_prop_active[animal_id == d_animal_id, date_dawn,],
          d_prop_active[animal_id == d_animal_id, night,],
          col = "blue")
    lines(d_prop_active[animal_id == d_animal_id, date_dawn,],
          d_prop_active[animal_id == d_animal_id, day_twi,],
          col = "orange")
    axis(2)
    axis(1, at = seq(x_start, x_end, by = "month"),
         labels = strftime(seq(x_start, x_end, by = "month"), format = "%Y-%m"),
         las = 2 )
    #  prop_active[[d_namimal_id]][,date_dawn],
    #                         as.POSIXct(d_day)+days(1), by = "hour"))
  } else {
    window_width = 7
    plot(d_prop_active[animal_id == d_animal_id, date_dawn,],
         rollapply(d_prop_active[animal_id == d_animal_id, total], width = window_width,
                   FUN = mean, partial = TRUE),
         type = "l", ylim = c(0,1), xlab = "Time", ylab = "PTA",
         axes = FALSE, xlim = c(x_start, x_end))
    abline(v=seq(x_start, x_end, by = "month"), lty = 2)
    lines(d_prop_active[animal_id == d_animal_id, date_dawn,],
          rollapply(d_prop_active[animal_id == d_animal_id, night,], width = window_width,
                    FUN = mean, partial = TRUE),
          col = "blue")
    lines(d_prop_active[animal_id == d_animal_id, date_dawn,],
          rollapply(d_prop_active[animal_id == d_animal_id, day_twi], width = window_width,
                    FUN = mean, partial = TRUE),
          col = "orange")
    axis(2)
    axis(1, at = seq(x_start, x_end, by = "month"),
         labels = strftime(seq(x_start, x_end, by = "month"), format = "%Y-%m"),
         las = 2 )
  }

},
d_animal_id = picker(as.list(as.character(d_prop_active[, unique(animal_id),]))),
d_smooth = checkbox(initial = FALSE))
}
