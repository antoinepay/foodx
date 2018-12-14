#' Algorithm in two steps:
#' first step complete missing values on the continuous predictor variables with 
#' an iterative PCA from missMDA package
#' second step is a linear regression trained on the dataset with the non missing nutrition score
#' and used to predict the missing ones
#'
#' @importFrom caret createDataPartition
#' @importFrom missMDA MIPCA
#' @importFrom stats glm predict
#'
#'
#'
#' @return a dataframe with barcode and corresponding grades
#' @export
#'
generate_missing_scores<-function(){
  food<-read_csv("data/food_with_matches.csv")
  
  
  b<-c("additives_n","ingredients_that_may_be_from_palm_oil_n","energy_100g",
       "saturated-fat_100g","fiber_100g","proteins_100g","sugars_100g","fat_100g","carbohydrates_100g",
       "salt_100g","sodium_100g","product_name","code","nutrition-score-fr_100g","nutrition_grade_fr")
  
  c<-c("energy_100g",
       "saturated-fat_100g","fiber_100g","proteins_100g","sugars_100g","fat_100g","carbohydrates_100g",
       "salt_100g","sodium_100g")
  
  food1<-food[,b]
  food1<- food1 %>% dplyr::filter(energy_100g<4000|is.na(energy_100g))
  food1<- food1 %>% dplyr::filter(fiber_100g<200|is.na(fiber_100g))
  food1<- food1 %>% dplyr::filter(sugars_100g<1000|is.na(sugars_100g))
  food1<- food1 %>% dplyr::filter(salt_100g<200|is.na(salt_100g))
  food1<-food1[rowSums(is.na(food1[,c])) < 9,]
  
  food2<-food1[,c]
  
  res.MIPCA <- MIPCA(as.matrix(food2), ncp = 2, nboot = 50)
  
  new_food_data<-as.data.frame(res.MIPCA$res.imputePCA)
  
  food1[,c]<-new_food_data
  
  pp<-is.na(food1$additives_n) 
  food1$additives_n[pp]<--1
  
  pp2<-is.na(food1$ingredients_that_may_be_from_palm_oil_n)
  food1$ingredients_that_may_be_from_palm_oil_n[pp2]<- -1
  
  
  
  
  
  food3<-food1 %>% dplyr::filter(!is.na(`nutrition-score-fr_100g`))
  food4<-food1 %>% dplyr::filter(is.na(`nutrition-score-fr_100g`))
  
  set.seed(3456)
  trainIndex <- createDataPartition(food3$additives_n, p = .8, 
                                    list = FALSE, 
                                    times = 1)
  
  
  
  Default_train<-food3[trainIndex,]
  Default_test<-food3[-trainIndex,]
  
  
  ll<-c(c,"nutrition-score-fr_100g","additives_n","ingredients_that_may_be_from_palm_oil_n")
  
  
  a<-glm(`nutrition-score-fr_100g`~.,data=food3[,ll])
  
  food3["new_grade"]<-0         
  food4["new_grade"]<-predict(a,food4[,ll])
  
  
  food1_bis<-base::rbind(food3,food4)
  
  
  food1_bis <- food1_bis %>% mutate(new_grade=if_else(-new_grade>5,-5,new_grade))
  food1_bis$`nutrition-score-fr_100g`[is.na(food1_bis$`nutrition-score-fr_100g`)]<-0
  
  food1_bis["final_grade"]<-food1_bis$`nutrition-score-fr_100g`+food1_bis$new_grade
  
  food1_bis<-food1_bis %>% select(c("code","final_grade"))
  
  write_rds(food1_bis,"new_grade.rds")
}