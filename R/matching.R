#' Create a dataframe that splits up the product name
#'
#' @param df 
#' @param col 
#' 
#' @importFrom purrr map keep
#'
#' @return data.frame
#' @export
prepare_df <- function(df, col_name) {
  col_name <- enquo(col_name)
  df <- df %>% 
    as_tibble() %>% 
    mutate(product_name_words = strsplit(tolower(!!col_name), "[\\ \\' \\,]"))
  
  df$product_name_words <- map(df$product_name_words, function(words) {
    keep(words, function(word) nchar(word) >= 3)
  })
  
  df
}

#' Insert columns in df created above with the matches between each column of the df above and the ingredient list
#'
#' @param df 
#' @param ingredient_vector 
#'
#' @return
#' @export
#'
build_df <- function(df, ingredient_vector) {
  
  df %>% 
    mutate(matches = map(product_name_words, function(words) {
      words %in% ingredient_vector 
    }))
}




# Working on bulding the decision rule that chooses which word to use to replace the product name, needs more exploration


#' Decision rule to choose which ingredient to use to replace the product name
#'
#' @return
#' @export
#'
replace <- function(){
  replaced_product_name <- ifelse(match1 != is.na(), match1, ifelse(match2 != is.na(), match2, match_last))
}





























