fetch_data_from_openfoodfacts <- function(force_download = FALSE) {
  
  food_csv_filepath <- 'data/food.csv'
  
  if (!file.exists(food_csv_filepath) | force_download) {
    download.file('https://fr.openfoodfacts.org/data/fr.openfoodfacts.org.products.csv', food_csv_filepath)
    food <- read_tsv(food_csv_filepath)
    
    columns_to_remove <- c('creator', 'created_datetime', 'last_modified_datetime', 'image_small_url')
    food <- food %>% filter(nchar(code) == 13) %>% select(-columns_to_remove)
    
    write_csv(food, food_csv_filepath)
  } else {
    food <- read_csv(food_csv_filepath)
  }
}
