library(stringr)
library(readr)
library(dplyr)
library(purrr)


#' Create a dataframe that splits up the product name
#'
#' @param df 
#' @param col 
#'
#' @return
#' @export
#'
#' @examples
prepare_df <- function(df,col){
  food_new <- df %>% 
    mutate(list_of_product_name_words = strsplit(df$col," ")) %>% 
      select(list_of_product_name_words)
  food_new <- food_new %>% mutate(list_of_product_name_words = sapply(food_new$list_of_product_name_words,tolower),
                                  total = sapply(food_new$list_of_product_name_words, function(x) length(unlist(strsplit(as.character(x), "\\W+")))),
                                  first_word = map(food_new$list_of_product_name_words,1),
                                  second_word = map(food_new$list_of_product_name_words,2),
                                  last_word = map(food_new$list_of_product_name_words,last))
return(food_new)
}

# Example:

prepare_df(test_name,product_name)



#' Insert columns in df created above with the matches between each column of the df above and the ingredient list
#'
#' @param df 
#' @param ingredient_vector 
#'
#' @return
#' @export
#'
#' @examples
build_df <- function(df,ingredient_vector){
  df$match1 <- str_extract(df$first_word,paste(ingredient_vector,collapse = "|")),
  df$match2 <- str_extract(df$second_word,paste(ingredient_vector,collapse = "|")),
  df$match_last <- str_extract(df$last_word,paste(ingredient_vector,collapse = "|")),
  return(df)
}


# Example:

build_df(food_new,Ingredients_List)




# Working on bulding the decision rule that chooses which word to use to replace the product name, needs more exploration


#' Decision rule to choose which ingredient to use to replace the product name
#'
#' @return
#' @export
#'
#' @examples
replace <- function(){
  replaced_product_name <- ifelse(match1 != is.na(), match1, ifelse(match2 != is.na(), match2, match_last))
}





























