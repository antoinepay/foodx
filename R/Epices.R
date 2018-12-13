#' Get the list of spices
#'
#' @import dplyr
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#'
#' @return vector
#' @export
#' 
my_page <- function(){
  x <- "https://fr.wikipedia.org/wiki/Herbes_et_aromates_de_cuisine"
  read_html(x) %>% 
  html_nodes("div.colonnes") %>%
  html_nodes("li") %>%
  html_text() %>% 
  as.vector()
}



