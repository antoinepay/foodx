#' List all the ingredients from the Marmiton recipe dataset
#' @return vector
#'
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' @importFrom readr read_csv
#' 
#' @export
ingredients_list <- function(){
  marmiton_recipes %>% 
    unnest_tokens(ingredient, recipeIngredients, token = 'regex', pattern=",") %>% 
    mutate(ingredient = gsub("-"," ",ingredient)) %>% 
    mutate(ingredient = tolower(ingredient)) %>% 
    distinct(ingredient) %>%
    as.vector()
}

#' List all the product names from the Open foods dataset
#' 
#' @return vector
#'
#' @import dplyr
#' @export
get_all_product_names <- function() {
  food_w_match_clean %>% dplyr::select(product_name) %>% as.vector()
}

#' Convert product name to generic ingredient
#'
#' @import dplyr
#' @param scanned_list list of products from database
#' @return vector of generic ingredients
#' 
#' @export
convert_to_ingredient_list <- function(scanned_list){
  food_w_match_clean %>% 
    filter(code %in% scanned_list) %>% 
    pull(qwe)
}
