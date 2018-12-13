#' Quickest recipes
#'
#' @param recipes tibble of recipes
#' @import dplyr
#' @return tibble of recipes sorted according to the total time
#' @export
#'
sort_time <- function(recipes) {
  recipes %>% 
    mutate(totalTime = recipePreparationTime + recipeCookingTime) %>% 
    arrange(totalTime)
}

#' Cheapest recipes
#'
#' @param recipes tibble of recipes
#' @import dplyr
#' @return tibble of recipes sorted according to the budget
#' @export
#'sor
sort_budget <- function(recipes) {
  recipes %>% 
    mutate(recipeCost = as.factor(recipeCost)) %>% 
    arrange(recipeCost)
}

#' Easiest recipes
#'
#' @param recipes tibble of recipes
#' @import dplyr
#' @return tibble of recipes sorted according to the budget
#' @export
#'
sort_difficulty <- function(recipes) {
  recipes %>% 
    mutate(recipeDifficulty = factor(recipeDifficulty, levels = c("tresfacile", "facile", "moyenne", "difficile"), ordered = TRUE)) %>% 
    arrange(recipeDifficulty)
}
