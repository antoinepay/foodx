.rs.api.documentSaveAll() # closes and saves all open files?li
suppressWarnings(lapply(paste('package:', names(sessionInfo()$otherPkgs), sep = ""), detach, character.only = TRUE, unload = TRUE))# detach all  packages
rm(list = ls(all.names = TRUE))# clean environneent
devtools::document('.') # create NAMESPACE and man
devtools::load_all('.') # load package
options(app.prod = FALSE) # TRUE = production mode, FALSE = development mode
shiny::runApp('inst/app') # run the main app
  