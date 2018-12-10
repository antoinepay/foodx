
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
      checkboxGroupInput(
        "filters",
        label = "Plus de filtres :", choices = c("Le plus rapide", "Le moins cher", "Le plus facile")
      ),
      actionButton(
        "button", label = "Go"
      )
    ),
    
    mainPanel(
      textOutput('title1'), 
      uiOutput('recipe1'), 
      textOutput('title2'), 
      uiOutput('recipe2'), 
      textOutput('title3'), 
      uiOutput('recipe3')
    )
  )
}



