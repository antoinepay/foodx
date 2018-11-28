
recipe_data <- read.csv("data/recipe_data.csv")#read recipe dataset


#' 
#'
#' @return vector
#'
#' @import dplyr
#' @import tidyverse
#' @import tidytext
#'
#' @export
ingredients_list <- function(){
  recipe_data_wide <- recipe_data %>% 
  unnest_tokens(ingredient, recipeIngredients,token = 'regex', pattern=",") %>% 
  mutate(ingredient = gsub("-"," ",ingredient))

all_ingredients <- unique(recipe_data_wide$ingredient)
return(all_ingredients)
}