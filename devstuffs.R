
options(usethis.full_name = "Antoine Payan")
usethis::use_mit_license()

usethis::use_package("dplyr")

usethis::use_package('readr')
usethis::use_package('rvest')
usethis::use_package('purrr')
usethis::use_package('xml2')
usethis::use_description()

devtools::document()

devtools::check()


