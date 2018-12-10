#' Top 2 quickest recipes
#'
#' @param tibble 
#'
#' @return
#' @export
#'
#' @examples
sort_time <- function(tibble){
  x <- numeric(tibble$recipePreparationTime+tibble$recipeCookingTime, ordered = TRUE)
  order(x) %>% 
    head()
}

#' Top 2 cheapest recipes
#'
#' @param tibble 
#'
#' @return
#' @export
#'
#' @examples
sort_budget <- function(tibble){
  z <- factor(tibble$recipeCost, levels = c("bonmarche", "moyen", "assezcher"), ordered = TRUE)
  order(z) %>% 
    head()
}

#' Top 2 easiest recipes
#'
#' @param tibble 
#'
#' @return
#' @export
#'
#' @examples
sort_difficulty<- function(tibble){
  y <- factor(tibble$recipeDifficulty, levels = c("tresfacile", "facile", "moyenne", "difficile"), ordered = TRUE)
  order(y) %>% 
    head()
}