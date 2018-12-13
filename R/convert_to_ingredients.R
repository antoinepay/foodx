#' Convert product names to ingredient lists
#'
#' @param scanned_list 
#' @import dplyr
#'
#' @return list
#' @export
#'
#' @examples 
convert_to_ingredient_list <- function(scanned_list){
  l <- food_w_match_clean %>% 
    filter(product_name %in% scanned_list) %>% 
    select(qwe)
  l <- as.list(l)
  
  return (l)
}