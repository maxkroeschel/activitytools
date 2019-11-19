#' Split column animal_tag
#'
#' \code{split_animaltag}  splits the column animal_tag in the columns
#'   'animal_id' and 'animal_tag' and adds them to the table.
#'
#' @param data A data.table with the column 'animal_tag'.
#' @return The original data.table with additional columns 'animal_id' and
#'   'tag_code'.
#' @examples
#' activity_data <- split_animaltag(activity_data)
#' @import data.table
#' @export


split_animaltag <- function(data) {
  data[,animal_id := data.table::tstrsplit(animal_tag, split = "_")[1],]
  data[,tag_code := data.table::tstrsplit(animal_tag, split = "_")[2],]
}
