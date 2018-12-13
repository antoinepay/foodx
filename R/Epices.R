#' Get the list of spices
#'
#' @param Noparam 
#' @import dplyr
#' @importFrom rvest read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#'
#' @return vector
#' @export
#'
#' @examples curry, safran, cumin, muscade, gingembre
#' 
my_page <- function(){
  x <- "https://fr.wikipedia.org/wiki/Herbes_et_aromates_de_cuisine"
  read_html(x) %>% 
  html_nodes("div.colonnes") %>%
  html_nodes("li") %>%
  html_text() %>% 
  as.vector()
}



