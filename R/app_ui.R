#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#' @import shiny
#' @export app_ui
app_ui <- function() {
  fluidPage(
    
    titlePanel("FoodX"),
    
    sidebarPanel(
      selectInput(
        "Ingredient 1", 
        label = "Ingredient 1", choices = ingredients_list(recipe_path), value = "carotte"
      ),
      textInput(
        "Ingredient 2", 
        label = "Ingredient 2", choices = ingredients_list(recipe_path), value = "poulet"
      ),
      textInput(
        "Ingredient 3", 
        label = "Ingredient 3", choices = ingredients_list(recipe_path), value = "champignons"
      ), 
      textInput(
        "Ingredient 4", 
        label = "Ingredient 4", choices = ingredients_list(recipe_path), value = "persil"
      ),
      textInput(
        "Ingredient 5", 
        label = "Ingredient 5", choices = ingredients_list(recipe_path), value = "abricots"
      ),
      sliderInput(
        "Time Cooking", "How long do you want to cook ?", 
        min = 0, max = 180, value = c(15,20), animate = TRUE, step = 1
      )
    ),
    
    mainPanel(
      tableOutput('recipes')
    )
  )
}



