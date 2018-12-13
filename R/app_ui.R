#' Shiny UI
#'
#' @import shiny
#' @import shinyBS
#' @export app_ui
app_ui <- function() {
  fluidPage(
    
    tags$head(
      tags$style(HTML("
                      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                      "))
      ),
    
    headerPanel(
      (h1("FoodX", 
          style = "font-family: 'Lobster', cursive;
        font-weight: 500; line-height: 1.1; 
          color: #5cb85c"))
    ),
    
    sidebarPanel(
      textInput(
        'barcodes',
        label = "Code-barres :"
      ),
      bsCollapse(id = "collapseCam",
                 bsCollapsePanel("Camera", tags$div(style="text-align:center",
                   HTML('<video style="width:100%;position:relative" id="vid"></video>'),
                   br(),
                   actionButton('snap', label = "Scan", class="btn btn-success")
                   ), style = "info")
      ),
      selectizeInput(
        "Ingredients", 
        label = "Ingrédients", choices = c(), 
        multiple = TRUE, options = list(placeholder = "Sélectionner les ingrédients")
      ),
      
      selectInput(
        "minimum_to_use",
        label = "Nombre minimum de vos ingrédients à utiliser :", choices = c()
      ),
      
      selectizeInput(
        "principal_ingredients",
        label = "Principaux ingrédients à utiliser :", choices = c(), 
        multiple = TRUE, options = list(placehoder = "Sélectionner les principaux ingrédients de votre recette")
      ),
      
      radioButtons(
        "filters",
        label = "Plus de filtres :", choices = c("Aucun", "Le plus rapide", "Le moins cher", "Le plus facile")
      ),
      actionButton(
        "button", label = "Go"
      )
    ),
    
    mainPanel(
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

