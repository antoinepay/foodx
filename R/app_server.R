#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#' @import shiny
#' @importFrom readr read_csv
#' @importFrom graphics hist
#' @importFrom stats rnorm
#' @export app_server
#'
app_server <- function(input, output,session) {
  output$recipes <- renderTable({
    
    marmiton_top_100 <- paste0('../../', foodX::output_marmiton_top_1OO)
    
    if (!file.exists(marmiton_top_100)) {
      withProgress(message = 'Fetching Marmiton', value = 0, {
        # Number of times we'll go through the loop
        
        links <- get_top_100_links()
        n <- length(links)
        
        dat <- data.frame(Title = numeric(0), Ingredient = numeric(0))
        
        for (i in 1:n) {
          # Each time through the loop, add another row of data. This is
          # a stand-in for a long-running computation.
          dat <- rbind(dat, fetch_recipe_detail(links[[i]]))
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Recipe", i))
          
        }
        
        write_csv(dat, marmiton_top_100)
      })
      
      dat
    } else {
      read_csv(marmiton_top_100)
    }
  
  })
}
