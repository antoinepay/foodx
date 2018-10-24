# Load all libraries

library(dplyr)
library(readr)

# Fetching and loading data
source('repository.R')

fetch_data_from_openfoodfacts(force_download = FALSE)
