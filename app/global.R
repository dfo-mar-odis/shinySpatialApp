# PACKAGES 
library(shiny)
library(shinyjs)
library(leaflet)
library(leafem)
library(mapedit)
library(sf)
library(glue)
library(shinyvalidate)


# load R scripts (including packages)
lapply(list.files("R/", pattern = ".[Rr]$", full.names = TRUE), source)
# load shiny modules
lapply(list.files("modules/", full.names = TRUE), source)


glue_path <- function(...) glue::glue(..., .sep = .Platform$file.sep)
glue_url <- function(...) glue::glue(..., .sep = "/")

add_lang_to_file <- function(files, lang, lowercase = TRUE) {
  if (lowercase) {
    # if length(lang) > 1, then other lang are ignored
    lang <- tolower(lang[1])
  } else {
    lang <- lang[1]
  }
  out <- c()
  for (i in files) {
    out <- c(
      out, 
      glue::glue(fs::path_ext_remove(i), "_", lang, ".", fs::path_ext(i))
    )
  }
  out
}



# clear previous report (if any)
clear_www_html()
clear_output()

# R log for dev purposes
msgInfo("R files loaded")
