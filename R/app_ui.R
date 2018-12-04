
#' Shiny UI
#'
#' @import shiny
#' @export app_ui
app_ui <- function() {
  fluidPage(
    
    titlePanel("FoodX"),
    
    sidebarPanel(
      selectizeInput(
        "Ingredients", 
        label = "Ingrédients", choices = c(), multiple = TRUE, options = list(placeholder = "Sélectionner les ingrédients")
      ),
      sliderInput(
        "Time Cooking", "Combien de temps souhaitez-vous cuisiner ?", 
        min = 0, max = 120, value = c(15,45), animate = TRUE, step = 1
      ),
      actionButton(
        "button", label = "Go"
      )
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
      tabPanel("Recettes sélectionnées pour vous", textOutput('title1'), tableOutput('recipe1'), textOutput('title2'), tableOutput('recipe2'), textOutput('title3'), tableOutput('recipe3')),
      tabPanel("Recettes Marmiton", tableOutput('marmiton_recipes'))
      )
    )
  )
}



