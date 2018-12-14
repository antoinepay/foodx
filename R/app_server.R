
#' Shiny Server
#'
#' @param input inputs from UI
#' @param output outputs to UI
#' @param session session to be used and observed
#'
#' @import shiny
#' @import dplyr
#' @importFrom readr read_csv
#' @importFrom graphics hist
#' @importFrom stats rnorm
#' @importFrom purrr map2_chr
#' @importFrom purrr imap_chr
#' @export
app_server <- function(input, output, session) {
  
  ingredients <- reactive({
    ingredients_list()$ingredient %>% 
      sort()
  })
  
  first_recipe <- eventReactive(input$button, {
    
    barcodes_ingredients <- union(
      convert_to_ingredient_list(strsplit(input$barcodes_input, split = ' ')), 
      convert_to_ingredient_list(input$barcodes)
    )
    
    generic_ingredients <- union(input$Ingredients, barcodes_ingredients)
    
    minimum_ingredients_to_use <- as.numeric(input$minimum_to_use)
    
    if (input$filters == "Aucun"){
      tibble <- get_best_recipes(generic_ingredients, minimum_ingredients_to_use = minimum_ingredients_to_use, must_include = input$principal_ingredients)$recipe_of_the_chef
    }
    else if (input$filters == "Le plus rapide"){
    tibble <- get_best_recipes(generic_ingredients, minimum_ingredients_to_use = minimum_ingredients_to_use, must_include = input$principal_ingredients)$recipe_of_the_chef %>% sort_time()
    }
    else if (input$filters == "Le moins cher"){
    tibble <- get_best_recipes(generic_ingredients, minimum_ingredients_to_use = minimum_ingredients_to_use, must_include = input$principal_ingredients)$recipe_of_the_chef %>% sort_budget()
    } 
    else if (input$filters == "Le plus facile"){
      tibble <- get_best_recipes(generic_ingredients, minimum_ingredients_to_use = minimum_ingredients_to_use, must_include = input$principal_ingredients)$recipe_of_the_chef %>% sort_difficulty()
    } 
   if (nrow(tibble) == 0){
     return("D\u00E9sol\u00E9, aucune recette ne correspond \u00E0 votre demande dans cette cat\u00E9gorie")
    } else {
      recipe_output(tibble)
    }
  }, ignoreInit = TRUE)
  
  first_msg <- eventReactive(input$button, {as.character(tags$div(class = "header", checked = NA, tags$hr(), tags$h2("Recettes du chef")))})
  
  second_recipe <- eventReactive(input$button, {
    
    barcodes_ingredients <- union(
      convert_to_ingredient_list(strsplit(input$barcodes_input, split = ' ')), 
      convert_to_ingredient_list(input$barcodes)
    )
    
    generic_ingredients <- union(input$Ingredients, barcodes_ingredients)
    
    minimum_ingredients_to_use <- as.numeric(input$minimum_to_use)
    
    if (input$filters == "Aucun"){
      tibble <- get_best_recipes(generic_ingredients, minimum_ingredients_to_use = minimum_ingredients_to_use, must_include = input$principal_ingredients)$recipe_using_most_ingedrient
    }
    else if (input$filters == "Le plus rapide"){
      tibble <- get_best_recipes(generic_ingredients, minimum_ingredients_to_use = minimum_ingredients_to_use, must_include = input$principal_ingredients)$recipe_using_most_ingedrient %>% 
        sort_time()
    }
    else if (input$filters == "Le moins cher"){
      tibble <- get_best_recipes(generic_ingredients, minimum_ingredients_to_use = minimum_ingredients_to_use, must_include = input$principal_ingredients)$recipe_using_most_ingedrient %>% 
        sort_budget()
    } 
    else if (input$filters == "Le plus facile"){
      tibble <- get_best_recipes(generic_ingredients, minimum_ingredients_to_use = minimum_ingredients_to_use, must_include = input$principal_ingredients)$recipe_using_most_ingedrient %>% 
        sort_difficulty()
    } 
    
    if (nrow(tibble) == 0){
      return("D\u00E9sol\u00E9, aucune recette ne correspond \u00E0 votre demande dans cette cat\u00E9gorie")
    } else {
      recipe_output(tibble)
    }
  }, ignoreInit = TRUE)
  
  second_msg <- eventReactive(input$button, {as.character(tags$div(class = "header", checked = NA, tags$hr(), tags$h2("Recettes avec le plus de vos ingr\u00E9dients")))})
  
  third_recipe <- eventReactive(input$button, {
   
    barcodes_ingredients <- union(
      convert_to_ingredient_list(strsplit(input$barcodes_input, split = ' ')), 
      convert_to_ingredient_list(input$barcodes)
    )
    
    generic_ingredients <- union(input$Ingredients, barcodes_ingredients)
    
    minimum_ingredients_to_use <- as.numeric(input$minimum_to_use)
    
    if (input$filters == "Aucun"){
      tibble <- get_best_recipes(generic_ingredients, minimum_ingredients_to_use = minimum_ingredients_to_use, must_include = input$principal_ingredients)$recipe_adding_least_ingredients
    }
    else if (input$filters == "Le plus rapide"){
      tibble <- get_best_recipes(generic_ingredients, minimum_ingredients_to_use = minimum_ingredients_to_use, must_include = input$principal_ingredients)$recipe_adding_least_ingredients %>% sort_time()
    }
    else if (input$filters == "Le moins cher"){
      tibble <- get_best_recipes(generic_ingredients, minimum_ingredients_to_use = minimum_ingredients_to_use, must_include = input$principal_ingredients)$recipe_adding_least_ingredients %>% sort_budget()
    } 
    else if (input$filters == "Le plus facile"){
      tibble <- get_best_recipes(generic_ingredients, minimum_ingredients_to_use = minimum_ingredients_to_use, must_include = input$principal_ingredients)$recipe_adding_least_ingredients %>% sort_difficulty()
    } 
    if (nrow(tibble) == 0){
      return("D\u00E9sol\u00E9, aucune recette ne correspond \u00E0 votre demande dans cette cat\u00E9gorie")
    }
    else {recipe_output(tibble)}
  }, ignoreInit = TRUE)
  
  third_msg <- eventReactive(input$button, {as.character(tags$div(class = "header", checked = NA, tags$hr(), tags$h2("Recettes avec le moins d'ingr\u00E9dients \u00E0 ajouter")))})
  
  observe({
    updateSelectizeInput(session = session, inputId = "Ingredients", choices = ingredients())
  })
  
  observe({
    updateSelectInput(session = session, inputId = "minimum_to_use", choices = 1:length(input$Ingredients))
  })
  
  observe({
    updateSelectizeInput(session = session, inputId = "principal_ingredients", choices = input$Ingredients)
  })

  output$title1 <- renderUI({
    HTML(first_msg()) 
  })
  
  output$recipe1 <- renderUI({
    req(first_recipe())
    HTML(first_recipe())
  })
  
  output$title2 <- renderUI({
    HTML(second_msg())
  })
  
  output$recipe2 <- renderUI({
    req(second_recipe())
    HTML(second_recipe())
})
  
  output$title3 <- renderUI({
    HTML(third_msg())
  })
  
  output$recipe3 <- renderUI({
    req(third_recipe())
    HTML(third_recipe())
  })
}

