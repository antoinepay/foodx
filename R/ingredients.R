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
    mutate(ingredient = tolower(ingredient))
    distinct(ingredient) %>%
    as.vector()
}
