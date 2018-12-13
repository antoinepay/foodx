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
#' @return vector
#'
#' @import dplyr
#' @export

product_names <- function(){
  
  dat <- data(food_w_match_clean)
  
  p <- dat %>% dplyr::select(product_name) %>% as.vector()
  
  return (p)
}




#' List all the ingredients from the Marmiton recipe dataset
#' @return list
#'
#' @import dplyr
#' @param scanned_list list of products from from database
#' 
#' 
#' @export

convert_to_ingredient_list <- function(scanned_list){
  l <- food_w_match_clean %>% 
    filter(product_name %in% scanned_list) %>% 
    select(qwe)
  l <- as.list(l)
  
  return (l)
}

