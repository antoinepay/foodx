context("expect_success")



test_that("expect = no error",{
  
  expect_is(filter_categories(food_test),"data.frame")
  
  expect_is(get_last_category("a,b,c"),"character")
  
  
  
  expect_is(sort_time(recipes_test),"data.frame")
  
  expect_is(sort_budget(recipes_test),"data.frame")
  
  expect_is(sort_difficulty(recipes_test),"data.frame")
  
  
  
  expect_is(function_div("URL", "recipeTitle", "pic_URL",
                         "recipeDifficulty", "recipeCost", "recipePreparationTime",
                         "recipeCookingTime", "health_index"),"character")
  
  expect_is(recipe_output(recipes_test),"character")
  
  
  expect_is(nutrition_score("fromage"), 'numeric')
  
  expect_is(recipe_score(recipes_test[1,]),"numeric")
  
  expect_is(score_column(recipes_test[1,]),"tbl_df")
  
  expect_is(category_health(recipes_test[1,]),"tbl_df")
  
  
  
  expect_is(ingredients_list(),"tbl_df")
  
  expect_is(convert_to_ingredient_list("0000000000123"),"character")
  
  
  expect_is(remove_ingredients_you_always_have(c("ail","chili")),"character")
  
  expect_is(get_subsets_from(c("ail","ail","chili")),"list")
  
  expect_is(get_number_for_one_combination(c("chili","fromage"),1),"numeric")
  
  expect_is(get.matrix.subset.containance("test"),"matrix")
  
  expect_is(preprocess_set("ail",1),"list")
  
  expect_error(get_position_of_future_empty(c("chili","fromage")))
  
  expect_error(get_number_of_each_combinations(c("chili","fromage")))
  
  expect_is(get_best_combinations(c("chili","fromage")),"character")
  
  expect_error(get_nb_best_combinations(c("ail","chili","fromage")))
  
  expect_error(get_best_recipes_du_chef(c("ail","chili","fromage")))
  
  expect_error(best_combination(c("ail","chili","fromage")))
  
  expect_error(get_best_recipe_using_most_ingredients(c("ail","chili","fromage")))
  
  expect_error(get_recipe_with_least_ingredients_to_add(c("ail","chili","fromage")))
  
  expect_is(get_best_recipes(c("ail","chili","fromage")),"list")
  
  
  
  
  
  


})
