library(shiny)
library(shinyjs)
library(leaflet)
library(leafem)
library(mapedit)
library(sf)
library(glue)
library(knitr) # reproducible reports from R Markdown 
library(rgdal) # read geospatial data files into report
library(maps) # uncouple lat/lon coordinates in sf objects
library(lubridate) # extract YEAR from columns containing dates
library(kableExtra) # build and manipulate tables
library(tidyverse) # a set of packages required for general data manipulation, including ggplot2 for plotting and dplyr
library(raster) # needed for manipulation of raster files
library(sf) # manipulation of simple features and to determine overlap of studyArea and databases
library(stringr) # manipulation of individual characters within strings in character vectors.
library(data.table) # manipulation of dataframes
library(gridExtra) # plot a grid of plots (priority cetacean habitat)
library(stars) # transform features and assign or modify coordinate reference systems in objects
library(ggspatial) #required for mapping
#next lines are necessary for the generation of the water mark on plots
#install.packages("remotes")
#remotes::install_github("terminological/standard-print-output")
library(standardPrintOutput) # required for watermarks on maps

## R messages 
msgInfo <- function(..., appendLF = TRUE) {
  txt <- paste(cli::symbol$info, ...)
  message(crayon::blue(txt), appendLF = appendLF)
  invisible(txt)
}

#' @describeIn msgInfo Reports an Error.
#' @export
msgError <- function(..., appendLF = TRUE) {
  txt <- paste(cli::symbol$cross, ...)
  message(crayon::red(txt), appendLF = appendLF)
  invisible(txt)
}

#' @describeIn msgInfo Reports a success.
#' @export
msgSuccess <- function(..., appendLF = TRUE) {
  txt <- paste(cli::symbol$tick, ...)
  message(crayon::green(txt), appendLF = appendLF)
  invisible(txt)
}

#' @describeIn msgInfo Reports a warning.
#' @export
msgWarning <- function(..., appendLF = TRUE) {
  txt <- paste(cli::symbol$warning, ...)
  message(crayon::yellow(txt), appendLF = appendLF)
  invisible(txt)
}

# HTML helper 

# Save formating for different help text 
myhelptxt <- function(x) {
  helpText(HTML(glue('<i class="fas fa-info-circle" aria-hidden="true"></i> 
  {x}')))
}

# horizontal space
hspace <- function(n) HTML(paste(rep("&nbsp;", n), collapse = ""))


## Check details

check_name <- function(x) {
  if (x != "") TRUE else FALSE
}

check_email <- function(x) {
  if (x != "") TRUE else FALSE
}

info_valid <- function(x, ok = TRUE, chk = FALSE) {
  cls <- ifelse(ok, "valid", "invalid")
  if (!chk) {
    renderUI(HTML(glue("<span class={cls}>{shiny::icon('info')} {x}</span>")))
  } else {
    renderUI(HTML(glue("<span class={cls}>{shiny::icon('info')} {x} 
      {shiny::icon('check')}</span>")))
  }
}


# do not include dots in new extension as it is added
switch_ext <- function(x, y) {
  ext <- tools::file_ext(x)
  sub(glue(".{ext}$"), glue(".{y}"), x) 
}

# clear extra HTML in www
clear_www_html <- function() {
  html_fl <- list.files("www", pattern = "*.html$", full.names = TRUE)
  file.remove(html_fl[html_fl != "www/empty_report.html"])
}

clear_output <- function() {
  fls <- list.files("output", full.names = TRUE)
  file.remove(fls[fls != "output/.gitkeep"])
}