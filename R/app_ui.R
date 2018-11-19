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
    mainPanel(
      tableOutput('recipes')
    )
  )
}
