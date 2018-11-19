
library(rvest)
library(dplyr)
library(purrr)
library(readr)

source('R/marmitoncrawler.R')

fetch_top_100_recipes()

d <- read_csv('data/marmition_100.csv')

options(usethis.full_name = "Antoine Payan")
usethis::use_mit_license()

usethis::use_package("dplyr")

usethis::use_package('readr')
usethis::use_package('rvest')
usethis::use_package('purrr')
usethis::use_package('xml2')
usethis::use_package('shiny')
usethis::use_description()

devtools::document()

devtools::check()

shiny_mon_app()

run_app()

fetch_top_100_recipes()

d <- read_csv('data/marmition_100.csv')
