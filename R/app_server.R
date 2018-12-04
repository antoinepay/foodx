
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
#' @export
app_server <- function(input, output, session) {
  
  ingredients <- reactive({
    ingredients_list()
  })
  
  first_recipe <- eventReactive(input$button, {
    get_best_recipies(input$Ingredients)$recipe_of_the_chef %>% select(-1)
  })
  
  first_msg <- eventReactive(input$button, {"Recettes du chef"})
  
  second_recipe <- eventReactive(input$button, {
    get_best_recipies(input$Ingredients)$recipe_using_most_ingedrient %>% select(-1)
  })
  
  second_msg <- eventReactive(input$button, {"Recettes avec le plus de vos ingrédients"})
  
  third_recipe <- eventReactive(input$button, {
    get_best_recipies(input$Ingredients)$recipe_adding_least_ingredients %>% select(-1)
  })
  
  third_msg <- eventReactive(input$button, {"Recettes avec le moins d'ingrédients à rajouter"})
  
  observe({
    updateSelectizeInput(session = session, inputId = "Ingredients", choices = ingredients())
  })
  
  output$marmiton_recipes <- renderTable({
    marmiton_recipes
  })
  
  output$title1 <- renderText({
    first_msg()
  })
  
  output$recipe1 <- renderTable({
    first_recipe()
  })
  
  output$title2 <- renderText({
    second_msg()
  })
  
  output$recipe2 <- renderTable({
   second_recipe()
})
  
  output$title3 <- renderText({
    third_msg()
  })
  
  output$recipe3 <- renderText({
    third_recipe()
  })

}
