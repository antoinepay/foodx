
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
  
  observe({
    updateSelectInput(session = session, inputId = "Ingredient1", choices = ingredients())
    updateSelectInput(session = session, inputId = "Ingredient2", choices = ingredients())
    updateSelectInput(session = session, inputId = "Ingredient3", choices = ingredients())
    updateSelectInput(session = session, inputId = "Ingredient4", choices = ingredients())
    updateSelectInput(session = session, inputId = "Ingredient5", choices = ingredients())
  })
  
  output$Recipes <- renderTable({
    marmiton_recipes
  })
}
