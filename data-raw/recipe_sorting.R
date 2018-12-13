#' Rank by Quickest recipes
#'
#' @param recipes output tibble of recipes in the shiny app 
#' @import dplyr
#' @return tibble of recipes sorted according to the total time required for the recipe
#' @export
#'
sort_time <- function(recipes) {
  recipes %>% 
    mutate(totalTime = recipePreparationTime + recipeCookingTime) %>% 
    arrange(totalTime)
}

#' Rank by Cheapest recipes
#'
#' @param recipes output tibble of recipes in the shiny app
#' @import dplyr
#' @return tibble of recipes sorted according to their cost 
#' @export
#'
sort_budget <- function(recipes) {
  recipes %>% 
    mutate(recipeCost = as.factor(recipeCost)) %>% 
    arrange(recipeCost)
}

#' Rank by Easiest recipes
#'
#' @param recipes output tibble of recipes in the shiny app
#' @import dplyr
#' @return tibble of recipes sorted according to the difficulty of preparation
#' @export
#'
sort_difficulty <- function(recipes) {
  recipes %>% 
    mutate(recipeDifficulty = factor(recipeDifficulty, levels = c("tresfacile", "facile", "moyenne", "difficile"), ordered = TRUE)) %>% 
    arrange(recipeDifficulty)
}
