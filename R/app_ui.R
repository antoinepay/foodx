
#' Shiny UI
#'
#' @import shiny
#' @export app_ui
app_ui <- function() {
  fluidPage(
    
    titlePanel("FoodX"),
    
    sidebarPanel(
      textInput(
        'barcodes',
        label = "Code-barres"
      ),
      selectizeInput(
        "Ingredients", 
        label = "Ingrédients", choices = c(), 
        multiple = TRUE, options = list(placeholder = "Sélectionner les ingrédients")
      ),
      
      textInput(
        "Code",
        label = "Entrer le code barre de votre produit :"
      ),
      
      selectInput(
        "minimum_to_use",
        label = "Combien de vos ingrédients voulez-vous utiliser au minimum ?", choices = c()
      ),
      
      selectizeInput(
        "principal_ingredients",
        label = "Principaux ingrédients à utiliser", choices = c(), 
        multiple = TRUE, options = list(placehoder = "Sélectionner les principaux ingrédients de votre recette")
      ),
      
      radioButtons(
        "filters",
        label = "Plus de filtres :", choices = c("Le plus rapide", "Le moins cher", "Le plus facile")
      ),
      actionButton(
        "button", label = "Go"
      )
    ),
    
    mainPanel(
      tags$div(
        HTML('<p id="barcode"></p>
              <button id="snap">Scan</button>
              <video id="vid"></video>')
      ),
      tags$script(src = "jquery.min.js"),
      tags$script(src = "webcam.js"),
      uiOutput('title1'), 
      uiOutput('recipe1'), 
      uiOutput('title2'), 
      uiOutput('recipe2'), 
      uiOutput('title3'), 
      uiOutput('recipe3')
    )
  )
}



