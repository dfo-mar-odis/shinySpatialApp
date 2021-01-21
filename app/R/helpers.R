library(shiny)
library(shinyjs)
library(leaflet)
library(leafem)
library(mapedit)
library(sf)
library(glue)

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


hspace <- function(n) HTML(paste(rep("&nbsp;", n), collapse = ""))

xtc_elmt <- function(x, y) unlist(lapply(x, `[`, y) )