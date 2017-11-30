#' Add column animal_tag
#'
#' \code{create_animaltag}  concatenates the columns 'animal_id' and
#'   'animal_tag' as identifier for the animal-tag-association.
#'
#' @param data A data.table with the columns 'animal_id' and 'tag_code'.
#' @return The original data.table with additional column 'animal_tag'.
#' @examples
#' activity_data <- create_animaltag(activity_data)
#' @import data.table
#' @export

create_animaltag <- function(data) {
  if (is.data.table(data) == FALSE) {
    stop("Data is not a data.table!")
  }
  data[,animal_tag := paste(animal_id, tag_code, sep = "_"),]
  return(data)
}
