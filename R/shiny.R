#' Run the Shiny App
#'
#' @return a Shiny App
#' @export
run_app <- function() {
  shinyApp(ui = app_ui(), server = app_server)
}
