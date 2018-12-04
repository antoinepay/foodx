library(stringr)
library(readr)
library(dplyr)

data <- read_csv("data/Ingredients_List")
food <- read_csv("data/food.csv")
name <- food %>% select(product_name)


# So far this finds the number of times we see x in the product name
find_matches <- function(x){
  t <- str_extract_all(name,x,simplify = TRUE)
  length(t)
}

# I'm now trying to nest this function inside a for loop for each element in the ingredients list we 
# obtained from marmiton

#It works if I define the vector with a few values:
p <- c("abricot","ail")
testing <- 0
for(i in p){
  testing[i] <- find_matches(i)
  print(testing[i])
}

# but I have a problem when I use data:

testing <- 0
for(i in data){
  testing[i] <- find_matches(i)
  print(testing[i])
}


  
