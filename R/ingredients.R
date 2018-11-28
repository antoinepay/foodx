recipe_path <- "data/recipe_data.csv"
#'
#' @return vector
#'
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' @importFrom readr read_csv
#' 
#' @export
ingredients_list <- function(path){
  recipe_data <- read_csv(path)
  recipe_data_wide <- recipe_data %>% 
    unnest_tokens(ingredient, recipeIngredients,token = 'regex', pattern=",") %>% 
    mutate(ingredient = gsub("-"," ",ingredient))
  all_ingredients <- unique(recipe_data_wide$ingredient)
  all_ingredients
}

ingredients_list(recipe_path)
