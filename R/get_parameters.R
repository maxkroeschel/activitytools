#' Get parameters
#'
#' \code{get_poarameters} extracts specified parameters from objects of class
#'   \code{activity} or \code{states}. An error is caused if any of the
#'   parameters is \code{NA}.
#'
#' @param x
#' @param parameters
#' @return  A named list with containing the values of the specified parameters
#' @examples
#' @import data.table

get_parameters <- function(x, parameters){
  if(!any(is(x, "activity"), is(x, "states"))){
    stop("Please provide an object of class 'activity' or 'states'")
  }
  ind_ex <- which(names(x$parameters) %in% parameters)
  par_ex <- x$parameters[ind_ex]
  if(any(is.na(par_ex))){
    missing <- names(par_ex[is.na(par_ex)])
    stop("Parameter(s) not specified: ", paste(missing, collapse = ", "))
  }
  if(length(par_ex) == 0){
    stop("No matching parameter(s) found.")
  }
  return(par_ex)
}
