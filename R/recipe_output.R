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
                        tags$h4(tags$a(href = URL, recipeTitle)), 
                        tags$table(tags$tr(
                          tags$td(tags$img(src = pic_URL, width = "150px", height = "150px", title = "Image de la recette", style = "border-radius:50%")),
                          tags$td(tags$ul(
                            tags$li(tags$p(glue("Difficult\u00E9 : {recipeDifficulty}"))), 
                            tags$li(tags$p(glue("Prix : {recipeCost}"))),
                            tags$li(tags$p(glue("Temps de pr\u00E9paration : {recipePreparationTime} minutes"))), 
                            tags$li(tags$p(glue("Temps de cuisson : {recipeCookingTime} minutes"))),
                            tags$li(tags$p(glue("Healthy ? {health_index}")))))
                          )
                        )
                      )
                )
}

#' Get a list of html tags, iterated over an entire dataframe
#'
#' @param tibble 
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