#' Top 2 quickest recipes
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
sort_time <- function(x){
  x <- marmiton_recipes$recipePreparationTime+marmiton_recipes$recipeCookingTime
  order(x) %>% 
    head(2)
}

#' Top 2 cheapest recipes
#'
#' @param z 
#'
#' @return
#' @export
#'
#' @examples
sort_budget <- function(z){
  z <- as.factor(marmiton_recipes$recipeCost)
  order(z) %>% 
    head(2)
}

#' Top 2 easiest recipes
#'
#' @param y 
#'
#' @return
#' @export
#'
#' @examples
sort_difficulty<- function(y){
  y <- as.factor(marmiton_recipes$recipeDifficulty)
  order(y) %>% 
    head(2)
}