url_titres_recettes <- 'https://www.marmiton.org/recettes/top-internautes-plat-principal.aspx'
output_marmiton_top_1OO <- 'data/marmiton_100.csv'

#' Fetch fromatted recipe ingredient list and preparation instruction steps
#'
#' @param url character
#'
#' @return data.frame
#' @import dplyr
#' @importFrom purrr map
#' @importFrom rvest html_text html_node html_nodes
#' @importFrom xml2 read_html
#' @export
#'
fetch_recipe_detail <- function(url) {
  html <- read_html(url) 
  ingredients <- html_nodes(html, ".recipe-ingredients__list__item") %>% 
    map(function(node) {
      paste0(html_text(html_node(node, '.recipe-ingredient-qt')), html_text(html_node(node, '.ingredient')))
    }) %>% 
    simplify()
  title <- html_text(html_node(html, '.main-title'))
  data.frame(Title = rep(c(title), times = length(ingredients)), Ingredient=ingredients, stringsAsFactors = F)
}


#' Fetch Top 100 recipes from Marmiton website
#'
#' @import dplyr
#' @importFrom rvest html_attr html_node html_nodes
#' @import readr
#' @importFrom purrr map simplify
#' @importFrom xml2 read_html
#' @export
#'
fetch_top_100_recipes <- function() {
  
  webpage <- read_html(url_titres_recettes)
  
  links <- webpage %>%
    html_nodes('.recipe-card-link') %>% 
    map(function(node) { html_attr(node, 'href') })
  
  df <- links %>%
    map(fetch_recipe_detail) %>% 
    bind_rows() %>% 
    write_csv(output_marmiton_top_1OO)
  
  output_filepath
}

#' Return Top 100 recipes links of Marmiton website
#'
#' @import dplyr
#' @importFrom rvest html_attr html_nodes
#' @importFrom purrr map 
#' @importFrom xml2 read_html
#' @export
#' @return data.frame
#'
get_top_100_links  <- function() {
  webpage <- read_html(url_titres_recettes)
  
  webpage %>%
    html_nodes('.recipe-card-link') %>% 
    map(function(node) { html_attr(node, 'href') })
}

#' Title
#'
#' @param df data.frame
#'
#' @return character
#' @export
#'
gather_recipes_to_csv <- function(df) {
  df <-  bind_rows() %>% 
    write_csv(output_marmiton_top_1OO)
  
  output_marmiton_top_1OO
}


