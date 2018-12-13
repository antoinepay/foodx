
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

  
  convert_to_ingredient_list <- function(scanned_list){
    l <- food_w_match_clean %>% 
      filter(product_name %in% scanned_list) %>% 
      select(qwe)
    l <- as.list(l)
    
    return (l)
  }
  
  
  ingredients <- reactive({
    ingredients_list()
  })
  
  first_recipe <- eventReactive(input$button, {
    req(input$Ingredients)
    #browser()
    if (is.null(input$filters)){
      tibble <- get_best_recipes(input$Ingredients, minimum_ingredients_to_use = input$minimum_to_use, must_include = input$principal_ingredients)$recipe_of_the_chef
    }
    else if (input$filters == "Le plus rapide"){
    tibble <- get_best_recipes(input$Ingredients, minimum_ingredients_to_use = input$minimum_to_use, must_include = input$principal_ingredients)$recipe_of_the_chef %>% sort_time()
    }
    else if (input$filters == "Le moins cher"){
    tibble <- get_best_recipes(input$Ingredients, minimum_ingredients_to_use = input$minimum_to_use, must_include = input$principal_ingredients)$recipe_of_the_chef %>% sort_budget()
    } 
    else if (input$filters == "Le moins cher"){
      tibble <- get_best_recipes(input$Ingredients, minimum_ingredients_to_use = input$minimum_to_use, must_include = input$principal_ingredients)$recipe_of_the_chef %>% sort_difficulty()
    } 
   titles <- tibble %>% select(2)
   urls <- tibble %>% select(1)
   vec <- map2_chr(titles$recipeTitle, urls$URL, ~as.character(a(.x, href = .y)))
   paste(vec, collapse = "<br/>")
  }, ignoreInit = TRUE)
  
  first_msg <- eventReactive(input$button, {as.character(tags$div(class = "header", checked = NA, tags$h4("Recettes du chef")))})
  
  second_recipe <- eventReactive(input$button, {
    req(input$Ingredients)
    #browser()
    if (is.null(input$filters)){
      tibble <- get_best_recipes(input$Ingredients, minimum_ingredients_to_use = input$minimum_to_use, must_include = input$principal_ingredients)$recipe_using_most_ingedrient
    }
    else if (input$filters == "Le plus rapide"){
      tibble <- get_best_recipes(input$Ingredients, minimum_ingredients_to_use = input$minimum_to_use, must_include = input$principal_ingredients)$recipe_using_most_ingedrient %>% sort_time()
    }
    else if (input$filters == "Le moins cher"){
      tibble <- get_best_recipes(input$Ingredients, minimum_ingredients_to_use = input$minimum_to_use, must_include = input$principal_ingredients)$recipe_using_most_ingedrient %>% sort_budget()
    } 
    else if (input$filters == "Le moins cher"){
      tibble <- get_best_recipes(input$Ingredients, minimum_ingredients_to_use = input$minimum_to_use, must_include = input$principal_ingredients)$recipe_using_most_ingedrient %>% sort_difficulty()
    } 
    titles <- tibble %>% select(2)
    urls <- tibble %>% select(1)
    vec <- map2_chr(titles$recipeTitle, urls$URL, ~as.character(a(.x, href = .y)))
    paste(vec, collapse = "<br/>")
  }, ignoreInit = TRUE)
  
  second_msg <- eventReactive(input$button, {as.character(tags$div(class = "header", checked = NA, tags$h4("Recettes avec le plus de vos ingrédients")))})
  
  third_recipe <- eventReactive(input$button, {
    req(input$Ingredients)
   # browser()
    if (is.null(input$filters)){
      tibble <- get_best_recipes(input$Ingredients, minimum_ingredients_to_use = input$minimum_to_use, must_include = input$principal_ingredients)$recipe_adding_least_ingredients
    }
    else if (input$filters == "Le plus rapide"){
      tibble <- get_best_recipes(input$Ingredients, minimum_ingredients_to_use = input$minimum_to_use, must_include = input$principal_ingredients)$recipe_adding_least_ingredients %>% sort_time()
    }
    else if (input$filters == "Le moins cher"){
      tibble <- get_best_recipes(input$Ingredients, minimum_ingredients_to_use = input$minimum_to_use, must_include = input$principal_ingredients)$recipe_adding_least_ingredients %>% sort_budget()
    } 
    else if (input$filters == "Le moins cher"){
      tibble <- get_best_recipes(input$Ingredients, minimum_ingredients_to_use = input$minimum_to_use, must_include = input$principal_ingredients)$recipe_adding_least_ingredients %>% sort_difficulty()
    } 
  }, ignoreInit = TRUE)
  
  third_msg <- eventReactive(input$button, {as.character(tags$div(class = "header", checked = NA, tags$h4("Recettes avec le moins d'ingrédients à ajouter")))})
  
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
    # browser()
    cat(first_recipe())
    HTML(first_recipe())
  })
  
  output$title2 <- renderUI({
    HTML(second_msg())
  })
  
  output$recipe2 <- renderUI({
    req(second_recipe())
    # browser()
    cat(second_recipe())
    HTML(second_recipe())
})
  
  output$title3 <- renderUI({
    HTML(third_msg())
  })
  
  output$recipe3 <- renderUI({
    req(third_recipe())
    #browser()
    cat(third_recipe())
    HTML(third_recipe())
  })

}

