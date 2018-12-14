#' Remove common ingredients such as salt, pepper, etc. 
#'
#' @param list list of ingredients
#'
#' @return list of filtered ingredients
#' @export
#'
remove_ingredients_you_always_have <- function(list){
  list[!(list %in% must_have_ingredients)]
}

#' Change dash to space
#'
#' @param x a string of character
#'
#' @return
#' @export
#'
#' @examples
remove_tiret<-function(x){
  x<-gsub("-"," ",x)
  x
}


#' a list of list of character
#'
#' @param list with all the recipes 
#'
#' @return
#' @export
#'
#' @examples
remove_tiret_for_marmiton<-function(){
  marmiton_recipes_ingredients<-lapply(marmiton_recipes_ingredients, remove_tiret)
  marmiton_recipes_ingredients
}

marmiton_recipes_ingredients<-remove_tiret_for_marmiton()

#' Quick way to obtain all the subsets given a set
#'
#' @importFrom purrr map
#' @param set original set of ingredients
#'
#' @return list of all possible subsets within the original one
#' @export
#'
get_subsets_from <- function(set) {
  n <- length(set)
  
  bin <- map(1:n, function(i) {
    rep.int(
      c(rep.int(F, 2L ^ (i - 1L)),
        rep.int(T, 2L ^ (i - 1L))),
      2L ^ (n - i)
    )
  })
  
  apply(do.call(cbind, bin), 1L, function(x) { set[x] } )[-1]
}

#' The function that will look if something is contained in something
#' if number of ingredients if too far (in percentage)
#' from the number of ingredients in the recipe, is_contained don't count the containance.
#' The percentage parameter is set to 0 by default for reasons you will understand after
#' 
#' @importFrom purrr map reduce
#' @import dplyr
#'
#' @param v1 vector to be included in v2
#' @param v2 vector to contain v1
#' @param percentage percentage of v2 covered by the elements of v2
#'
#' @return boolean 
#' @export
#'
is_contained <- function(v1, v2, percentage = 0) {
  
  minimum_length <- length(v2) * percentage / 100 
  
  if (length(v1) < minimum_length) {    
    return(FALSE)                         
  }
  
  all(v1 %in% v2)
}

#' Count the number of appearances of one combination of ingredients in the list of marmiton recipes
#' 
#' @import dplyr
#' @importFrom purrr map reduce
#' 
#' @param one_combination one particular combination in all the subset
#' @param percentage a percentage
#'
#' @export
get_number_for_one_combination <- function(one_combination, percentage) {
  map(
    marmiton_recipes_ingredients, 
    function(recipe_ingredients) {
      ifelse(is_contained(one_combination, recipe_ingredients, percentage), 1, 0)
    }) %>% 
    reduce(sum)
}

#' Calcul matrix of containance between different sets: for a given set i, m[i,j]=1 if the set j contains the set i
#'
#' @param all.subset all subsets of ingredients you have
#'
#' @return matrix of containance
#' @export
#'
get.matrix.subset.containance<-function(all.subset)
{
  matrix.subset<-matrix(nrow = length(all.subset),ncol=length(all.subset))
  
  for (i in 1:length(all.subset)){
    matrix.subset[i,]<-sapply(all.subset, is_contained, v1=all.subset[[i]])*1
  }
  
  matrix.subset
}


#' Give all the subsets of the set according to the conditions wanted (minimum ingredient to use, must include ...)
#'
#' @param set the ingredient you have in your fridge
#' @param minimum_to_use a minimum of ingredient you want to use
#' @param must_include the ingredient that must be in the recipe
#'
#' @return subsets arranged in increasing length
#' @export
#'
preprocess_set <- function(
  set,
  minimum_to_use,
  must_include = NULL
){
  all_subsets <- get_subsets_from(set)
  
  if (!is.null(must_include)) {
    all_subsets<-all_subsets[sapply(all_subsets, is_contained,v1=must_include)]
  }
  
  all_subsets<-all_subsets[lapply(all_subsets, length) >= minimum_to_use]
  all_subsets<-all_subsets[order(sapply(all_subsets,length),decreasing=F)]
  all_subsets
}

#' Enables to iterate only on the subset that don't contain a subset with no appearances in the list of unique
#' That is why we set percentage to 0 in the is_contained function.
#' Indeed if it wasn't, we would not iterate on set that contains a subset with no appearance in the list of unique.
#' Yet having no appearances in that list can mean, that the percentage rule was not passed,
#' and not because it doesn't appear.
#'
#' @param matrix a matrix with the subset in row and in column
#' @param i the row index
#'
#' @export
#'
get_position_of_future_empty<-function(matrix,i){
  
  position<-rep(0,length(matrix[i,]))
  
  for (j in 1:length(matrix[i,])){
    
    if (matrix[i,j]==1){
      position[j]<-j
    }
  }
  position[position>0]
}

#' Give the number of appearances of each subset in the list of unique
#' 
#'
#' @param set the set of ingredient you have
#' @param minimum_to_use in the recipe
#' @param must_include ingredient 
#'
#' @export
#'
get_number_of_each_combinations<-function(set, minimum_to_use, must_include = NULL) {
  
  all_subsets <- preprocess_set(set, minimum_to_use, must_include)
  
  a<-rep(0,length(all_subsets))
  
  matrix<-get.matrix.subset.containance(all_subsets)
  
  for (i in 1:length(all_subsets)){
    
    if(matrix[i,i]!=0){
      a[i]<-get_number_for_one_combination(all_subsets[[i]],0)
      
      if (a[i]==0){
        index<-get_position_of_future_empty(matrix,i)
        diag(matrix)[index]<-0
      }
      
    }
    
  }
  list(all_subsets, a)
}



#' Returns the subsets that have one or more appearances in the list of marmiton's ingredients
#'
#' @param list list of subsets of ingredients
#'
#' @export
#' @return list of subsets usable in some recipes
#'
get_best_combinations<-function(list){
  index_vector <- list[[2]] > 0
  list[[1]][index_vector]
}

#' returns the number of appearance of each interresting subset in the list of unique, 
#' with taking into account - this time - the percentage of ingredient that cover the recepies 
#' (e.g. if recipie is carotte oeuf fromage, the subset "carotte" covers 33\% of the recipie)
#' if percentage demanded is 40\% than the subset "carotte" won't appear.
#'
#' @importFrom purrr map
#' 
#' @param all_subsets all possible subsets from the original list of ingredients
#' @param percentage miminum percentage of the recipe to be covered by the subset of ingredients 
#'
#' @export
#' @return list of occurences for each subset
get_nb_best_combinations<-function(all_subsets, percentage){
  
  nb_occurences <- map(all_subsets, function(subset) {
    get_number_for_one_combination(subset, percentage)
  })
  
  list(ingredients = all_subsets, nb_occurences = nb_occurences)
}

#' Calculate the best combination of ingredients
#' i.e the one that appears the most in the list of unique, with respect to the parameters
#' and returns a list of recipes containing this combination.
#' Test with get_best_recipes_du_chef(c("carotte", "jambon", "fromage"), 20, 1, T)
#'
#' @importFrom glue glue
#' @import dplyr
#'
#' @param ingredients_to_use set of ingredients to use in our search
#' @param proportion_of_recipe proportion of the recipe covered by the subset of ingredients
#' @param minimum_ingredients_to_use number of minimum ingredients to use in the recipe
#' @param must_include list of mandatory ingredients to use
#'
#' @return List of recipes
#' @export
#'
get_best_recipes_du_chef <- function(
  ingredients_to_use, 
  proportion_of_recipe, 
  minimum_ingredients_to_use, 
  must_include
){
  new_minimum_to_use<-minimum_ingredients_to_use
  
  best_combinations <- try(get_number_of_each_combinations(
    ingredients_to_use, 
    new_minimum_to_use,
    must_include) %>% 
      get_best_combinations() %>% 
      get_nb_best_combinations(proportion_of_recipe),silent = T)
  
  
  while (length(best_combinations$ingredients) ==0){
    
    new_minimum_to_use<-new_minimum_to_use-1
    
    best_combinations <- try(get_number_of_each_combinations(
      ingredients_to_use, 
      new_minimum_to_use,
      must_include) %>% 
        get_best_combinations() %>% 
        get_nb_best_combinations(proportion_of_recipe),silent = T)
  }
  
  best_combination <- best_combinations$ingredients[which.max(best_combinations$nb_occurences)][[1]]
  
  indexes <- marmiton_recipes_ingredients %>% 
    map_lgl(function(ingredients) {
      is_contained(best_combination, ingredients, proportion_of_recipe)
    })
  
  if (new_minimum_to_use!=minimum_ingredients_to_use){
    print(glue("Sorry , we could not use {minimum_ingredients_to_use+1} ingredients, 
                     used {new_minimum_to_use} instead for the recipe of the chef"))
  }
  
  if (dim(marmiton_recipes[indexes,])[1]==0) {
    print(glue("Sorry the Chef has nothing for you to cover {proportion_of_recipe}% of the recipe"))}
  
  marmiton_recipes[indexes,]
  
  }


#' Calculate the biggest combination of ingredient that appears at least one time
#' and returns the corresponding receipies
#'
#' @import dplyr
#' @importFrom purrr map_lgl
#' 
#' @param ingredients_to_use set of ingredients to use in our search
#' @param must_include list of mandatory ingredients to use 
#'
#' @return list of recipes 
#' @export
#'
get_best_recipe_using_most_ingredients <- function(
  ingredients_to_use, 
  must_include
){
  
  best_combinations <- get_number_of_each_combinations(
    ingredients_to_use,
    minimum_to_use=0,
    must_include
  ) %>% 
    get_best_combinations() %>% 
    get_nb_best_combinations(0)
  
  best_combination <- best_combinations$ingredients[which.max(sapply(best_combinations$ingredients,length))][[1]]
  
  indexes <- marmiton_recipes_ingredients %>% 
    map_lgl(function(ingredients) {
      is_contained(best_combination, ingredients,0)
    })
  
  marmiton_recipes[indexes,]
}

#' Calculate the combination that is the closest to the combination of one recipe
#' and returns the corresponding recipe
#' 
#' @importFrom glue glue
#' @importFrom purrr map_lgl
#' @import dplyr
#' 
#' @importFrom glue glue
#' @importFrom purrr map_lgl
#' @param ingredients_to_use in the recipe
#' @param minimum_ingredients_to_use in the recipe
#' @param must_include ingredient in the recipe
#'
#' @return list of recipes
#' @export
#'
get_recipe_with_least_ingredients_to_add<-function(
  ingredients_to_use, 
  minimum_ingredients_to_use, 
  must_include
){
  
  new_minimum_to_use<-minimum_ingredients_to_use
  
  best_combinations <- get_number_of_each_combinations(
    ingredients_to_use, 
    new_minimum_to_use,
    must_include
  ) %>% 
    get_best_combinations() 
  
  while (length(best_combinations) == 0){
    
    new_minimum_to_use<-new_minimum_to_use-1
    
    best_combinations <- get_number_of_each_combinations(
      ingredients_to_use, 
      new_minimum_to_use,
      must_include) %>% 
      get_best_combinations()
  }
  
  t<-5
  v<-100
  
  best_combination_bis<- best_combinations %>% get_nb_best_combinations(v)
  
  while (all((best_combination_bis$nb_occurences)==0)){
    v<-v-t
    best_combination_bis<- best_combinations %>% get_nb_best_combinations(v)
    
  }
  best_combination <- best_combination_bis$ingredients[which.max(best_combination_bis$nb_occurences)][[1]]
  
  indexes <- marmiton_recipes_ingredients %>% 
    map_lgl(function(ingredients) {
      is_contained(best_combination, ingredients,v)
    })
  
  if (new_minimum_to_use!=minimum_ingredients_to_use){
    print(glue("Sorry, we could not use {minimum_ingredients_to_use+1} ingredients, 
                     used {new_minimum_to_use+1} instead for the third type receipe"))
  }
  
  marmiton_recipes[indexes,]
  }



#' Give the list of all the recipes given by the three functions called, depending on some criteria
#'
#' @param ingredients_to_use list of ingredients to use (in our fridge for example)
#' @param proportion_of_recipe proportion of the recipe covered by the subset of the ingredients
#' @param minimum_ingredients_to_use minimum number of ingredients to really use within the ingredients_to_use 
#' @param must_include ingredients to be included
#'
#' @export
#' @return list of recipes
#'
get_best_recipes<-function(
  ingredients_to_use, 
  proportion_of_recipe = 30, 
  minimum_ingredients_to_use = 0, 
  must_include = NULL)
  {
  
  recipe_of_the_chef <- get_best_recipes_du_chef(
    ingredients_to_use, 
    proportion_of_recipe, 
    minimum_ingredients_to_use, 
    must_include
  )
  
  recipe_using_most_ingredients <- get_best_recipe_using_most_ingredients(
    ingredients_to_use, 
    must_include
  )
  
  recipe_adding_least_ingredients <- get_recipe_with_least_ingredients_to_add(
    ingredients_to_use, 
    minimum_ingredients_to_use, 
    must_include
  )
  
  list(
    recipe_of_the_chef = recipe_of_the_chef,
    recipe_using_most_ingedrient = recipe_using_most_ingredients,
    recipe_adding_least_ingredients = recipe_adding_least_ingredients
  )
}
