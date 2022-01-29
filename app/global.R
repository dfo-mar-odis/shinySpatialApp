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


lapply(list.files("modules/", full.names = TRUE), source)



glue_path <- function(...) glue::glue(..., .sep = .Platform$file.sep)
glue_url <- function(...) glue::glue(..., .sep = "/")
