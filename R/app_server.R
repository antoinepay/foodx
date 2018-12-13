
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
      tibble <- get_best_recipies(input$Ingredients)$recipe_of_the_chef
    }
    else if (input$filters == "Le plus rapide"){
    tibble <- get_best_recipies(input$Ingredients)$recipe_of_the_chef %>% sort_time()
    }
    else if (input$filters == "Le moins cher"){
    tibble <- get_best_recipies(input$Ingredients)$recipe_of_the_chef %>% sort_budget()
    } 
    else if (input$filters == "Le moins cher"){
      tibble <- get_best_recipies(input$Ingredients)$recipe_of_the_chef %>% sort_difficulty()
    } 
   titles <- tibble %>% select(2)
   urls <- tibble %>% select(1)
   vec <- map2_chr(titles$recipeTitle, urls$URL, ~as.character(a(.x, href = .y)))
   paste(vec, collapse = "<br/>")
  }, ignoreInit = TRUE)
  
  first_msg <- eventReactive(input$button, {"Recettes du chef"})
  
  second_recipe <- eventReactive(input$button, {
    req(input$Ingredients)
    #browser()
    if (is.null(input$filters)){
      tibble <- get_best_recipies(input$Ingredients)$recipe_using_most_ingedrient
    }
    else if (input$filters == "Le plus rapide"){
      tibble <- get_best_recipies(input$Ingredients)$recipe_using_most_ingedrient %>% sort_time()
    }
    else if (input$filters == "Le moins cher"){
      tibble <- get_best_recipies(input$Ingredients)$recipe_using_most_ingedrient %>% sort_budget()
    } 
    else if (input$filters == "Le moins cher"){
      tibble <- get_best_recipies(input$Ingredients)$recipe_using_most_ingedrient %>% sort_difficulty()
    } 
    titles <- get_best_recipies(input$Ingredients)$recipe_using_most_ingedrient %>% select(2)
    urls <- get_best_recipies(input$Ingredients)$recipe_using_most_ingedrient %>% select(1)
    vec <- map2_chr(titles$recipeTitle, urls$URL, ~as.character(a(.x, href = .y)))
    paste(vec, collapse = "<br/>")
  }, ignoreInit = TRUE)
  
  second_msg <- eventReactive(input$button, {"Recettes avec le plus de vos ingr\u00E9dients"})
  
  third_recipe <- eventReactive(input$button, {
    req(input$Ingredients)
    #browser()
    if (is.null(input$filters)){
      tibble <- get_best_recipies(input$Ingredients)$recipe_adding_least_ingredients
    }
    else if (input$filters == "Le plus rapide"){
      tibble <- get_best_recipies(input$Ingredients)$recipe_adding_least_ingredients %>% sort_time()
    }
    else if (input$filters == "Le moins cher"){
      tibble <- get_best_recipies(input$Ingredients)$recipe_adding_least_ingredients %>% sort_budget()
    } 
    else if (input$filters == "Le moins cher"){
      tibble <- get_best_recipies(input$Ingredients)$recipe_adding_least_ingredients %>% sort_difficulty()
    } 
    titles <- get_best_recipies(input$Ingredients)$recipe_adding_least_ingredients %>% select(2)
    urls <- get_best_recipies(input$Ingredients)$recipe_adding_least_ingredients %>% select(1)
    vec <- map2_chr(titles$recipeTitle, urls$URL, ~as.character(a(.x, href = .y)))
    paste(vec, collapse = "<br/>")
  }, ignoreInit = TRUE)
  
  third_msg <- eventReactive(input$button, {"Recettes avec le moins d'ingr\u00E9dients \u00E0 rajouter"})
  
  observe({
    updateSelectizeInput(session = session, inputId = "ingredients", choices = ingredients())
  })

  output$title1 <- renderText({
    first_msg() 
  })
  
  output$recipe1 <- renderUI({
    req(first_recipe())
    # browser()
    cat(first_recipe())
    HTML(first_recipe())
  })
  
  output$title2 <- renderText({
    second_msg()
  })
  
  output$recipe2 <- renderUI({
    req(second_recipe())
    # browser()
    cat(second_recipe())
    HTML(second_recipe())
})
  
  output$title3 <- renderText({
    third_msg()
  })
  
  output$recipe3 <- renderUI({
    req(third_recipe())
    # browser()
    cat(second_recipe())
    HTML(second_recipe())
  })

}

