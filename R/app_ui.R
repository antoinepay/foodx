
#' Shiny UI
#'
#' @import shiny
#' @export app_ui
app_ui <- function() {
  fluidPage(
    
    titlePanel("FoodX"),
    
    sidebarPanel(
      selectInput(
        "Ingredient1", 
        label = "Ingredient 1", choices = c()
      ),
      selectInput(
        "Ingredient2", 
        label = "Ingredient 2", choices = c()
      ),
      selectInput(
        "Ingredient3", 
        label = "Ingredient 3", choices = c()
      ), 
      selectInput(
        "Ingredient4", 
        label = "Ingredient 4", choices = c()
      ),
      selectInput(
        "Ingredient5", 
        label = "Ingredient 5", choices = c()
      ),
      sliderInput(
        "Time Cooking", "Combien de temps souhaitez-vous cuisiner ?", 
        min = 0, max = 180, value = c(15,20), animate = TRUE, step = 1
      )
    ),
    
    mainPanel(
      tableOutput('Recipes')
    )
  )
}



