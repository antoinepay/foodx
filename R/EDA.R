library(readr )
library(dplyr)
library(readr)

Sys.setlocale(locale="en_us.UTF-8")

data<-read_csv("data/food.csv") 
dim(data)
dim(na.omit(data))

data<-data %>% filter(!is.na(data$product_name)) #remove rows without product name
data<-data %>% filter(countries_tags=="en:france") #remove rows from other countries
data<-data %>% select(-which(colMeans(is.na(.)) == 1)) #remove columns with NA only
data<-data %>% select(-c("created_t","last_modified_t","packaging","packaging_tags",
                         "manufacturing_places","manufacturing_places_tags","first_packaging_code_geo",
                         "purchase_places","stores","image_url","image_ingredients_url",
                         "image_ingredients_small_url", "image_nutrition_url","image_nutrition_small_url")
                         
                         
data<-data %>% select(-c("traces_tags",                                
                         "traces_fr",                                  
                         "serving_size",                               
                         "traces",                                     
                         "emb_codes",                                  
                         "emb_codes_tags", 
                         "cities_tags",
                         "origins",
                         "origins_tags"
                         )) #remove useless columns 

c<-sapply(data$categories_fr,function(x) strsplit(x, ",")[[1]][length(strsplit(x, ",")[[1]])]) 
#select the most specific category after the last comma

e<-rep(0,length(c))
for (i in 1:length(c)){
  e[i]<-c[[i]]
}
#transorm  ugly list into nice vector

data['categorie_simple']<-e


#select variable that have less than 100 categories for analysis 
a<-rep(0,dim(data)[2])
for (i in 1:dim(data)[2]){
  a[i]<-dim(unique(data[,i]))[1]<100
}
a<-as.logical(a)

#create list with unique values and number of values for each unique 
dataframe_analyse<-sapply(data[, a],function(x) aggregate(data.frame(count = x), list(value = x), length))

#enhence the list by adding the right names
b<-rep("b",length(dataframe_analyse))
for (i in 1:(length(dataframe_analyse)/2)){
  b[2*i-1]<-glue::glue("{colnames(data[, a])[i]}_unique")
  b[2*i]<-glue::glue("{colnames(data[, a])[i]}_count")
}

names(dataframe_analyse)<-b


#remove columns with more than 80% of missing values:
data<-data %>% select(which(colMeans(is.na(data))<0.8))


