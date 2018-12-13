#' Get the average nutrition score of an ingredient 
#'
#' @param ingredient an ingredient
#'
#' @return
#' @export

nutrition_score <- function(ingredient){
  df <- food %>% filter(matches == ingredient) 
  avg <- mean(df$`nutrition-score-fr_100g`, na.rm = TRUE)
  return(avg)
}




#' Takes a list of ingredients and returns the average nutrition grade
#'
#' @param list_of 
#'
#' @return returns the average nutrition score for a list of ingredients
#' @export
recipe_score <- function(list_of){ 
  
  
  list_of <- unnest_tokens(list_of,ingredient, recipeIngredients, token = 'regex', pattern=",") %>% 
    mutate(ingredient = gsub("-"," ",ingredient)) %>% 
    mutate(ingredient = tolower(ingredient)) %>% 
    as.vector()
  
  
  avg <- lapply(list_of$ingredient,nutrition_score)
  sum <- mean(as.numeric(avg),na.rm = TRUE)
  return(sum)
}



#' Creates a column in the marmiton recipes dataframe with the average overall nutritional score for all ingredients used.
#'
#' @param marmiton_recipes a dataframe with the information about each recipe
#'
#' @return returns the dataframe used as an input with an extra column 
#' @export
score_column <- function(marmiton_recipes){
  
  scores <- numeric()
  for (i in c(1:dim(marmiton_recipes)[1])){
    
    
    scores <- append(scores,recipe_score(marmiton_recipes[i,]))
    
  }
  
  marmiton_recipes$scores <- scores
  return(marmiton_recipes)
  
}


