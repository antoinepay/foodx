recipe_tibble <- function(string){

if (input$filters == "Aucun"){
  tibble <- get_best_recipes(input$Ingredients, minimum_ingredients_to_use = input$minimum_to_use, must_include = input$principal_ingredients)$recipe_of_the_chef
}
else if (input$filters == "Le plus rapide"){
  tibble <- get_best_recipes(input$Ingredients, minimum_ingredients_to_use = input$minimum_to_use, must_include = input$principal_ingredients)$recipe_of_the_chef %>% sort_time()
}
else if (input$filters == "Le moins cher"){
  tibble <- get_best_recipes(input$Ingredients, minimum_ingredients_to_use = input$minimum_to_use, must_include = input$principal_ingredients)$recipe_of_the_chef %>% sort_budget()
} 
else if (input$filters == "Le plus facile"){
  tibble <- get_best_recipes(input$Ingredients, minimum_ingredients_to_use = input$minimum_to_use, must_include = input$principal_ingredients)$recipe_of_the_chef %>% sort_difficulty()
} 
return(tibble)  
}