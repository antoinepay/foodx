#' Creates a matching column for the OpenFoodData set with Marmiton generic ingredients
#'
#' @import dplyr
#' @param df food dataset from OpenFoodData
#'
#' @return data.frame
#' @export
#'
match_open_food_data_to_marmiton_ingredients <- function(df) {
  ingredients <- ingredients_list()$ingredient
  df %>% 
    split_columns_to_match %>% 
    match_list_column_to_ingredients(ingredients, product_name_words, product_name_matches) %>% 
    match_list_column_to_ingredients(ingredients, categories_words, categories_matches) %>% 
    match_list_column_to_ingredients(ingredients, categories_fr_words, categories_fr_matches) %>% 
    concat_matches(product_name_matches, categories_matches, categories_fr_matches) %>% 
    select(-ends_with('_matches')) %>% 
    select(-ends_with('_words')) %>% 
    unnest(matches)
}


#' Splits mutliple columns into list columns for possible matching 
#' 
#' @import dplyr
#' @param df data.frame with strings
#'
#' @return data.frame
#' @export
#'
split_columns_to_match <- function(df) {
  df %>% 
    split_column_words_to_new_col(product_name, product_name_words) %>% 
    split_column_words_to_new_col(categories, categories_words) %>%
    split_column_words_to_new_col(categories_fr, categories_fr_words)
}


#' Create a dataframe that splits up the words of column_name to words_column_name
#'
#' @param df  df to apply the split on
#' @param column_name column to split
#' @param words_column_name  name of new column
#' 
#' @import dplyr
#' @importFrom purrr map keep
#'
#' @return data.frame
#' @export
split_column_words_to_new_col <- function(df, column_name, words_column_name) {
  words_column_name <- enquo(words_column_name)
  column_name <- enquo(column_name)
  df %>% 
    as_tibble() %>% 
    mutate(!!words_column_name := strsplit(tolower(!!column_name), "[\\ \\' \\,]"),
           !!words_column_name := map(!!words_column_name, function(words) {
              keep(words, ~ nchar(.) >= 3)
             })
          )
}

#' Match list_column words with marmiton ingredients list
#' Create a matches_column
#'
#' @import dplyr
#' @importFrom purrr map
#' 
#' @param df food dataframe
#' @param ingredient_vector ingredient list
#' @param list_column name of the column to split into matches column
#' @param matches_column column with the matched ingredient corresponding to the product
#'
#' @return data.frame
#' @export
#'
match_list_column_to_ingredients <- function(df, ingredient_vector, list_column, matches_column) {
  list_column <- enquo(list_column)
  matches_column <- enquo(matches_column)
  df %>% mutate(!!matches_column := map(!!list_column, intersect, ingredient_vector))
}

#' Concat matches by unioning list columns
#'
#' @import dplyr
#' @importFrom purrr pmap reduce map
#' @param df food dataframe from OpenFoodFacts
#' @param ... matches column names
#' 
#'
#' @return data.frame with list of ingredient matches
#' @export
#'
concat_matches <- function(df, ...) {
  matches_columns <- quos(...)
  df %>% 
    mutate(matches = pmap(df %>% select(!!!matches_columns), ~ reduce(., union, .init = c()))) %>% 
    filter(matches %>% map(length) > 0)
}