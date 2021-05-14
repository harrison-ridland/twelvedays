#' Produces the string for the entire song
#'
#' @param dataset A data frame containing information about gifts
#' contains the gift phrases
#'
#' @return A string singing the line of the song with all gifts for the given day.
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export

sing_a_song = function(data){
  data = data %>%
  mutate(
    Full.Phrase = pmap_chr(data, ~make_phrase(..1, numify(..1), ..3, ..4, ..5, ..6))
  )
  return(sapply(1:12, function(y) sing_day(data, y, "Full.Phrase")))
}
