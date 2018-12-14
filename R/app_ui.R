#' Shiny UI
#'
#' @import shiny
#' @import shinyBS
#' @import htmltools
#' @export app_ui
app_ui <- function() {
  htmlDependency(name = 'foodX-assets', 
                 package = 'foodX', 
                 version = packageVersion('foodX'), 
                 src = '.', 
                 script = 'webcam.js'
                 )
  fluidPage(
    tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');")),
      
    titlePanel(
        '', windowTitle = "FoodX"
    ),
    
    h1('FoodX', style = "font-family: 'Lobster';font-weight: 500; line-height: 1.1; color: #5cb85c"),
    
    sidebarPanel(
      textInput(
        'barcodes_input',
        label = "Code-barres :",
        placeholder = "S\u00E9parer les code-barres par un espace"
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
        label = "Ingr\u00E9dients", choices = c(), 
        multiple = TRUE, options = list(placeholder = "S\u00E9lectionner les ingr\u00E9dients")
      ),
      
      selectInput(
        "minimum_to_use",
        label = "Nombre minimum de vos ingr\u00E9dients \u00E0 utiliser :", choices = c()
      ),
      
      selectizeInput(
        "principal_ingredients",
        label = "Principaux ingr\u00E9dients \u00E0 utiliser :", choices = c(), 
        multiple = TRUE, options = list(placehoder = "S\u00E9lectionner les principaux ingr\u00E9dients de votre recette")
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
      tags$script(src = 'webcam.js'),
      uiOutput('title1'), 
      uiOutput('recipe1'), 
      uiOutput('title2'), 
      uiOutput('recipe2'), 
      uiOutput('title3'), 
      uiOutput('recipe3')
    )
  )
}

