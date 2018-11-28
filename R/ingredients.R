#' List all the ingredients from Marmiton
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
    distinct(ingredient) %>%
    as.vector()
}
