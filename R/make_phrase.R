#' Puts the various parts of speech together into a full phrase.
#'
#' @param num An integer
#' @param num_word A string corresponding to the integer
#' @param item A string
#' @param verb A string
#' @param adjective A string
#' @param location A string
#'
#' @return A string containing the words in grammatical order.
#'
#' @import stringr
#' @import glue
#' @import dplyr
#' @import purrr
#'
#' @export


make_phrase <- function(num, num_word, item, verb, adjective, location){
  if (num != 1){
    item <- pluralize_gift(item)
  }
  num_word = numify(num)
  verb <- str_replace_na(verb, "")
  adjective <- str_replace_na(adjective, "")
  location <- str_replace_na(location, "")
  combined = str_c(num_word, adjective, item, verb, location, sep = " ") 
  combined = str_replace_all(combined, "  ", " ")
  combined = str_replace(combined, " $", "")
  return(combined)
}
numify = function(number){
  if (number == 1){
    num = "a"
  } else if (number == 2){
    num = "Two"
  } else if (number == 3){
    num = "Three"
  }else if (number == 4){
    num = "Four"
  }else if (number == 5){
    num = "Five"
  }else if (number == 6){
    num = "Six"
  }else if (number == 7){
    num = "Seven"
  }else if (number == 8){
    num = "Eight"
  }else if (number == 9){
    num = "Nine"
  }else if (number == 10){
    num = "Ten"
  }else if (number == 11){
    num = "Eleven"
  }else if (number == 12){
    num = "Twelve"
  }
}

test_that("test making a phrase",{
  expect_error(make_phrase(1, "First", "Apple", NA, NA, "in a boars mouth"))
})
