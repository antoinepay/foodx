# No Remotes ----
# Attachments ----
to_install <- c("dplyr", "glue", "purrr", "readr", "rvest", "shiny", "shinyBS", "stringr", "tidytext", "xml2")
  for (i in to_install) {
    message(paste("looking for ", i))
    if (!requireNamespace(i)) {
      message(paste("     installing", i))
      install.packages(i)
    }

  }
