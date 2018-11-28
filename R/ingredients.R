
library(dplyr)
library(tidyverse)
library(tidytext)

recipe_data <- read.csv("data/recipe_data.csv")#read recipe dataset


recipe_data_wide <- recipe_data %>% 
  unnest_tokens(ingredient, recipeIngredients,token = 'regex', pattern=",") %>% 
  mutate(ingredient = gsub("-"," ",ingredient))

all_ingredients <- unique(recipe_data_wide$ingredient)
