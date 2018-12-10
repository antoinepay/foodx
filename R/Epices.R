#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
my_page <- function(x){
  x <- "https://fr.wikipedia.org/wiki/Herbes_et_aromates_de_cuisine"
  read_html(x) %>% 
  html_nodes("div.colonnes") %>%
  html_nodes("li") %>%
  html_text() %>% 
  as.vector()
}



