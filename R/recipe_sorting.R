#' Top 2 quickest recipes
#'
#' @param tibble 
#' @import dplyr
#' @return
#' @export
#'
#' @examples
sort_time <- function(tibble){
  new_tibble <- tibble %>% mutate(totalTime = recipePreparationTime + recipeCookingTime)
  x <- new_tibble$totalTime %>% 
  order() 
  new_tibble$totalTime[x]
}


#' Top 2 cheapest recipes
#'
#' @param tibble 
#' @import dplyr
#' @return
#' @export
#'
#' @examples
sort_budget <- function(tibble){
  z <- factor(tibble$recipeCost, levels = c("bonmarche", "moyen", "assezcher"), ordered = TRUE) %>% 
  order()
  tibble$recipeCost[z]
}

#' Top 2 easiest recipes
#'
#' @param tibble 
#' @import dplyr
#' @return
#' @export
#'
#' @examples
sort_difficulty<- function(tibble){
  y <- factor(tibble$recipeDifficulty, levels = c("tresfacile", "facile", "moyenne", "difficile"), ordered = TRUE) %>% 
  order()
  tibble$recipeDifficulty[y]
}
