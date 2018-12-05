remove_ingredients_you_always_have <- function(list){
  list<-list[! (list %in% must_have_ingredients)]
  list
}

#' Quick way to obtain all the subsets given a set
#'
#' @param set
#'
#' @return list of subsets
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
#' from the number of ingredients in the receipe, is_contained don't count the containance.
#' The percentage parameter is set to 0 by default for reasons you will understand after
#' @importFrom purrr map reduce
#' @import dplyr
#'
#' @param v1 vector to be included in v2
#' @param v2 vector to contain v1
#' @param percentage 
#'
#' @return boolean
#' @export
#'
is_contained <- function(v1, v2, percentage = 0) {
  
  minimum_length <- length(v2) * percentage / 100 
  
  if (length(v1) < minimum_length) {    
    return(FALSE)                         
  }
  
  map(v1, function(val) {
    val %in% v2
  }) %>% reduce(all)
}

#' Count the number of appearances of one combination of ingredients in the list of marmiton recipes
#' @import dplyr
#' @importFrom purrr map reduce
#' @param one_combination 
#' @param percentage 
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
#' @param all.subset 
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
#' @param set 
#' @param minimum_to_use 
#' @param usual.ing 
#' @param must_include 
#'
#' @return subsets arranged in increasing length
#' @export
#'
preprocess_set <- function(
  set,
  minimum_to_use,
  must_include
){
  all_subsets <- get_subsets_from(set)
  
  if (must_include!="nothing_bro") {
    all_subsets<-all_subsets[sapply(all_subsets, is_contained,v1=must_include)]
  }
  all_subsets<-all_subsets[lapply(all_subsets, length) > minimum_to_use]
  all_subsets<-all_subsets[order(sapply(all_subsets,length),decreasing=F)]
  all_subsets
}

#' Enables to iterate only on the subset that don't contain a subset with no appearances in the list of unique
#' That is why we set percentage to 0 in the is_contained function.
#' Indeed if it wasn't, we would not iterate on set that contains a subset with no appearance in the list of unique.
#' Yet having no appearances in that list can mean, that the percentage rule was not passed,
#' and not because it doesn't appear.
#'
#' @param matrix 
#' @param i 
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
#' @param set 
#' @param minimum_to_use 
#' @param must_include 
#'
#' @export
#'
get_number_of_each_combinations<-function(set,minimum_to_use,must_include){
  
  all_subsets <- preprocess_set(set, minimum_to_use,must_include)
  
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
  list(all_subsets,a)
}



#' returns the subsets that have one or more appearance in the list of unique 
#'
#' @param list 
#'
#' @export
#'
get_best_combinations<-function(list){
  index_vector<-list[[2]]>0
  list[[1]][index_vector]
}

#' returns the number of appearance of each interresting subset in the list of unique, 
#' with taking into account - this time - the percentage of ingredient that cover the recepies 
#' (e.g. if recipie is carotte oeuf fromage, the subset "carotte" covers 33\% of the recipie)
#' if percentage demanded is 40\% than the subset "carotte" won't appear.
#'
#' @param all_subsets 
#' @param percentage 
#'
#' @export
get_nb_best_combinations<-function(all_subsets, percentage){
  
  nb_occurences <- map(all_subsets, function(subset) {
    get_number_for_one_combination(subset, percentage)
  })
  
  list(ingredients = all_subsets, nb_occurences = nb_occurences)
}

#' Calculate the best combination of ingredients
#' i.e the one that appears the most in the list of unique, with respect to the parameters
#' and returns a list of recipies containing this combination.
#' Test with get_best_recipes_du_chef(c("carotte", "jambon", "fromage"), 20, 1, T)
#'
#' @param ingredients_to_use Set of ingredients
#' @param proportion_of_recipe 
#' @param minimum_ingredients_to_use 
#' @param must_include 
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
  
  
  while (class(best_combinations)[1] == "try-error"){
    
    new_minimum_to_use<-new_minimum_to_use-1
    
    best_combinations <- try(get_number_of_each_combinations(
      ingredients_to_use, 
      new_minimum_to_use,
      
      must_include) %>% 
        get_best_combinations() %>% 
        get_nb_best_combinations(proportion_of_recipe),silent = T)
  }
  
  best_combination <- best_combinations$ingredients[which.max(best_combinations$nb_occurences)][[1]]
  
  indexes <- get_list_of_unique() %>% 
    map_lgl(function(ingredients) {
      is_contained(best_combination, ingredients, proportion_of_recipe)
    })
  
  if (new_minimum_to_use!=minimum_ingredients_to_use){
    print(glue::glue("Sorry bro, we could not use {minimum_ingredients_to_use+1} ingredients, 
                     used {new_minimum_to_use+1} instead for the receipe of the chef"))
  }
  
  marmiton_recipes[indexes,]
  }




#' Calculate the biggest combination of ingredient that appears at least one time
#' and returns the corresponding receipies
#'
#' @param ingredients_to_use 
#' @param must_include 
#'
#' @return list of recipes 
#' @export
#'
get_best_receipe_using_most_ingredients<-function(
  ingredients_to_use, 
  must_include
){
  best_combinations <- get_number_of_each_combinations(
    ingredients_to_use,
    minimum_to_use=0,
    
    must_include) %>% 
    get_best_combinations() %>% 
    get_nb_best_combinations(0)
  
  best_combination <- best_combinations$ingredients[which.max(sapply(best_combinations$ingredients,length))][[1]]
  
  indexes <- get_list_of_unique() %>% 
    map_lgl(function(ingredients) {
      is_contained(best_combination, ingredients,0)
    })
  
  marmiton_recipes[indexes,]
}





#' Calculate the combination that is the closest to the combination of one receipe
#' and returns the corresponding receipe
#' 
#' @param ingredients_to_use 
#' @param minimum_ingredients_to_use 
#' @param must_include 
#'
#' @return list of recipes
#' @export
#'
get_receipe_with_least_ingredients_to_add<-function(
  ingredients_to_use, 
  minimum_ingredients_to_use, 
  must_include
){
  
  new_minimum_to_use<-minimum_ingredients_to_use
  
  best_combinations <- get_number_of_each_combinations(
    ingredients_to_use, 
    new_minimum_to_use,
    must_include) %>% 
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
  
  indexes <- get_list_of_unique() %>% 
    map_lgl(function(ingredients) {
      is_contained(best_combination, ingredients,v)
    })
  
  if (new_minimum_to_use!=minimum_ingredients_to_use){
    print(glue::glue("Sorry bro, we could not use {minimum_ingredients_to_use+1} ingredients, 
                     used {new_minimum_to_use+1} instead for the third type receipe"))
  }
  
  marmiton_recipes[indexes,]
  }



#' Give the list of all the recipes given by the three functions called
#'
#' @param ingredients_to_use 
#' @param proportion_of_recipe 
#' @param minimum_ingredients_to_use 
#' @param must_include 
#'
#' @export
#'
get_best_recipies<-function(
  ingredients_to_use, 
  proportion_of_recipe=30, 
  minimum_ingredients_to_use=0, 
  must_include="nothing_bro"){
  
  recipe_of_the_chef<-get_best_recipes_du_chef(
    ingredients_to_use, 
    proportion_of_recipe, 
    minimum_ingredients_to_use, 
    must_include)
  
  recipe_using_most_ingedrient<-get_best_receipe_using_most_ingredients(
    ingredients_to_use, 
    must_include)
  
  recipe_adding_least_ingredients<-get_receipe_with_least_ingredients_to_add(
    ingredients_to_use, 
    minimum_ingredients_to_use, 
    must_include)
  
  
  my_list<-list(recipe_of_the_chef=recipe_of_the_chef,
                recipe_using_most_ingedrient=recipe_using_most_ingedrient,
                recipe_adding_least_ingredients=recipe_adding_least_ingredients)
  
  my_list
}
