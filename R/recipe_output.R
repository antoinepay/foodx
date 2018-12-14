#' Recipes output in HTML for the Shiny App 
#'
#' @param URL url to be put in href
#' @param recipeTitle title to be put in <a>
#' @param pic_URL URL of the recipe's image
#' @param recipeDifficulty difficulty of the recipe
#' @param recipeCost cost of the whole recipe
#' @param recipePreparationTime preparation time for the recipe
#' @param recipeCookingTime cooking time for the recipe
#' @param health_index Healthiness of the recipe
#'
#' @return html tag
#' @export
#' 
#' @examples function_div('url', 'title', 'picture_url', 'difficulty', 'cost', 'time_p', 'time_c', 'health')
#'
function_div <- function(
  URL, 
  recipeTitle,
  pic_URL, 
  recipeDifficulty,
  recipeCost,
  recipePreparationTime, 
  recipeCookingTime, 
  health_index
) {
  as.character(tags$div(class = "recipe", checked = NA,
                        tags$h4(tags$a(href = URL, recipeTitle, target='blank')), 
                        tags$table(tags$tr(
                          tags$td(tags$img(src = pic_URL, width = "150px", height = "150px", title = "Image de la recette", style = "border-radius:50%")),
                          tags$td(tags$ul(
                            tags$li(tags$p(glue("<b>Difficult\u00E9</b> : {recipeDifficulty}"))), 
                            tags$li(tags$p(glue("<b>Prix</b> : {recipeCost}"))),
                            tags$li(tags$p(glue("<b>Temps de pr\u00E9paration</b> : {recipePreparationTime} minutes"))), 
                            tags$li(tags$p(glue("<b>Temps de cuisson</b> : {recipeCookingTime} minutes"))),
                            tags$li(tags$p(glue("<b>Healthy</b> ? {health_index}")))))
                          )
                        )
                      )
                )
}

#' Get a list of html tags, iterated over an entire dataframe
#'
#' @param tibble recipes to be transformed in html
#'
#' @return html tag
#' @export
#'
recipe_output <- function(tibble){
  vec <- tibble %>% 
    select(c(URL, recipeTitle, pic_URL, recipeDifficulty, recipeCost, recipePreparationTime, recipeCookingTime, health_index)) %>%
    mutate (recipeDifficulty = ifelse(recipeDifficulty == "tresfacile", "tr\u00E8s facile", recipeDifficulty)) %>% 
    mutate(recipeCost = ifelse(recipeCost == "bonmarche", "bon march\u00E9", recipeCost)) %>% 
    pmap(function_div)
  paste(vec, collapse = "<br/>")
}