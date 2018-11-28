recipe_path <- "data/recipe_data.csv"
#' 
#'
#' @return vector
#'
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' 
#' @export

ingredients_list <- function(path){
  recipe_data <- read.csv(path)
  recipe_data_wide <- recipe_data %>% 
  unnest_tokens(ingredient, recipeIngredients,token = 'regex', pattern=",") %>% 
  mutate(ingredient = gsub("-"," ",ingredient))

all_ingredients <- unique(recipe_data_wide$ingredient)
return(all_ingredients)
}
