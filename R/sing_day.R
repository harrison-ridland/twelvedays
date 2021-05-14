#' Produces the string for one day of the song.
#'
#' @param dataset A data frame containing information about gifts
#' @param line The number of the line for the day you want to sing about
#' @param phrase_col The variable name for the column in the dataset that
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
song_str <- function(days, phrase, line){
  if (line != 1 & days == 1){
    saying = str_c("and ", phrase[days])
  }else{
    return(phrase[days])
  }
  
}
sing_day <- function(dataset, line, phrase_col){
  phrases <- dataset %>% pull({{phrase_col}})
  dsing = str_c("On the ",dataset$Day.in.Words[line], " day of Christmas, my true love sent to me:" )
  singvec = c(dsing, sapply(line:1, function(x) song_str(x, phrase = phrases, line = line))) 
  singvec = glue::as_glue(singvec)
return(singvec)
}
