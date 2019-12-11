#' Plot states
#'
#' \code{plot_states} plots the active states and proportion of time in state
#'   active for each animal.
#'
#' @param states An object of class \code{activity}.
#' @param threshold Type of thresholds. Must be one of \code{"a"}, \code{"b"}, \code{"c"}.
#' @examples
#' plot_states(active_states = active_states_a,
#'             prop_time_active = prop_time_active,
#'             gps = gps_data)
#'
#' @import data.table
#' @export

plot_states <- function(active_states,
                        prop_time_active,
                        gps = NULL){
 oldpar <- par(no.readonly=TRUE)
  # on.exit(par(oldpar))
  #

  if(all(is.na(gps))){
    gps <- NULL
  }


  manipulate::manipulate({
    d_animal_id <- unlist(strsplit(d_animal_tag, split = "_"))[1]
    d_tag_code <- unlist(strsplit(d_animal_tag, split = "_"))[2]

    tmp_active_states <- active_states[animal_tag ==d_animal_tag,,]
    tmp_prop_time_active <- prop_time_active[animal_tag ==  d_animal_tag,,]

    # split an active state that spans over 24:00 into two states
    for (i in 1 : nrow(tmp_active_states)){
      if (as.Date(tmp_active_states$to_active[i]) != as.Date(tmp_active_states$end_active[i])) {
        tmp_active_states <- rbind(tmp_active_states,
                                   data.frame("to_active"= lubridate::floor_date(tmp_active_states$end_active[i], "day"),
                                              "end_active"= tmp_active_states$end_active[i]
                                              ), fill = TRUE)
        tmp_active_states$end_active[i] <-
          lubridate::floor_date(tmp_active_states$end_active[i], "day")-lubridate::seconds(1)}
    }



    # create temporary sequence for plotting

    date_seq <- tmp_active_states[,unique(as.Date(to_active)),]

    # plot
    layout(matrix(c(1,1,1,1,2), 1, 5, byrow = TRUE))
    par(mar = c(5.1, 6.1, 4.1, 0))
    plot(0,0, xlim = c(0,60*24),
         ylim = c(as.integer(min(date_seq))-0.5, as.integer(max(date_seq))+0.5),
         type = "n", axes=FALSE, xlab = "Time", ylab = "")
    title(main = paste("Animal ID: ", d_animal_id,
                      "   -   Tag code: ",d_tag_code, sep = ""),
          cex.main = 1.5)

      tmp_month <- seq(lubridate::ceiling_date(min(date_seq), unit = "months"),
                       lubridate::floor_date(max(date_seq), unit = "months"), by = "month")
      abline(h=  as.integer(tmp_month), col = "black", lty=3)
      axis(1, at = seq(0,1440, by = 120),
           labels = paste(seq(0,24,by = 2),rep("00",12), sep =":"))
      axis(2,at =  c(min(date_seq), min(tmp_month)),
           labels= F, col.ticks = "white",
           line = -0.5)
      axis(2,at =  c(max(date_seq), max(tmp_month)),
           labels= F, col.ticks = "white",
           line = -0.5)
      axis(2,at =  as.integer(tmp_month),
           labels= F,
           line = -0.5)
      axis(2, as.integer(tmp_month), tick = F,
           labels = tmp_month, las = 2, line = -0.7)

    for (i in 1:length(date_seq)){
      rect(xleft = ts2minutes(tmp_active_states$to_active[
        as.Date(tmp_active_states$to_active)== date_seq[i]]),
        ybottom = as.integer(date_seq[i])-0.45 ,
        xright =ts2minutes(tmp_active_states$end_active[
          as.Date(tmp_active_states$to_active)== date_seq[i]]),
        ytop = as.integer(date_seq[i])+0.45,
        col = "blue",
        border = NA)
      }
    lines( ts2minutes(sort(tmp_prop_time_active$ts_sr)),
           as.integer(as.Date(sort(tmp_prop_time_active$ts_sr))), lwd = 3,
           col = adjustcolor( "yellow", alpha.f = 0.9))
    lines( ts2minutes(sort(tmp_prop_time_active$ts_ss)),
           as.integer(as.Date(sort(tmp_prop_time_active$ts_ss))), lwd = 3,
           col = adjustcolor( "yellow", alpha.f = 0.9))
    lines( ts2minutes(sort(tmp_prop_time_active$ts_dusk)),
           as.integer(as.Date(sort(tmp_prop_time_active$ts_dawn))), lwd = 3,
           col = adjustcolor( "orange", alpha.f = 0.9))
    lines( ts2minutes(sort(tmp_prop_time_active$ts_dawn)),
           as.integer(as.Date(sort(tmp_prop_time_active$ts_dawn))), lwd = 3,
           col = adjustcolor( "orange", alpha.f = 0.9))
    rect(0,min(date_seq)-0.45,1440,max(date_seq+0.45))

    if (add_gps == TRUE) {
      if (is.null(gps)) {
        message("You have to provide a data.table with the GPS data!")
      } else {
        temp_gps <- gps[animal_tag == d_animal_tag,,]
        if (nrow(temp_gps) > 0) {
          points(x = temp_gps[,ts2minutes(ts),],
                 y = temp_gps[,as.integer(as.Date(ts)),],
                 col = "green", pch = 19, cex = 0.2)
          } else {message("No GPS data for this animal available!")}
        }
      }

    par(mar = c(5.1, 0.5, 4.1, 1))
    plot(0,0, xlim = c(0,100),
         ylim = c(as.integer(min(date_seq))-0.5, as.integer(max(date_seq))+0.5),
         type = "n", axes=FALSE, xlab = "PTA (%)", ylab = "")

      abline(h=  as.integer(tmp_month), col = "black", lty=3)

    for (i in seq(20,100,by = 20)) {lines(c(i,i),c(min(date_seq), max(date_seq)), lty=3)}
    #abline(v=seq(0,1,by = 0.2), col = "black", lty=3)
    # legend(x = c(-10,100), y = c(min(date_seq),min(date_seq)+lubridate::days(3)), yjust=0,
    #        legend = c("total", "day", "night"),
    #        fill = c("black", "orange", "blue"), horiz = TRUE,
    #        cex = 0.9, bg = "white", box.col = "white", text.width = 2)

    if ("date_dawn" %in% colnames(tmp_prop_time_active)) {
    lines(tmp_prop_time_active$day *100 ,tmp_prop_time_active$date_dawn, type = "b",
          pch = 20, cex =0.5, col = "orange", lwd = 1)
    lines(tmp_prop_time_active$night *100 ,tmp_prop_time_active$date_dawn, type = "b",
          pch = 20,  cex =0.5, col = "blue", lwd = 1)
    lines(tmp_prop_time_active$total *100 ,tmp_prop_time_active$date_dawn, type = "b",
          pch = 20, cex =0.5, col = "black", lwd = 1)} else
      if ("date_sr" %in% colnames(tmp_prop_time_active)) {
        lines(tmp_prop_time_active$day *100 ,tmp_prop_time_active$date_sr, type = "b",
              pch = 20, cex =0.5, col = "orange", lwd = 1)
        lines(tmp_prop_time_active$night *100 ,tmp_prop_time_active$date_sr, type = "b",
              pch = 20,  cex =0.5, col = "blue", lwd = 1)
        lines(tmp_prop_time_active$total *100 ,tmp_prop_time_active$date_sr, type = "b",
              pch = 20, cex =0.5, col = "black", lwd = 1)}
    axis(1)
    rect(0,min(date_seq)-0.45,100,max(date_seq+0.45))
    par(mar = oldpar$mar, mfrow = oldpar$mfrow)
    },
    d_animal_tag = manipulate::picker(as.list(active_states[,unique(animal_tag),])),
    add_gps = manipulate::checkbox(label = "add GPS " ,initial = FALSE)
  )
}
