#' Set parameters for object of class \code{activity}
#'
#' \code{set_poarameters} takes a named list of parameter values and sets the
#' matching parameters in an object of class \code{activity} or \code{states} to the
#' corresponding values. (i.e. \code{NA})
#'
#' @param x
#' @param parameters
#' @return  An object of class \code{activity} or \code{states}
#' @examples
#' @import data.table

set_parameters <- function(x,
                           parameters){

  if(!any(is(x, "activity"), is(x, "states"))){
    stop("Please provide an object of class 'activity' or 'states'")
  }

  par_supl <- names(parameters)
  par_set <- names(x$parameters[names(x$parameters)
                                       %in% par_supl])
  if(length(par_set) > 0){
    for(i in 1:length(par_set)){
      ind_ap <- which(names(x$parameters) == par_set[i])
      ind_sp <- which(names(parameters) == par_set[i])
      x$parameters[ind_ap] <- parameters[ind_sp]
    }
  }
  return(x)
}
