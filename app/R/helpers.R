library(shiny)
library(shinyjs)
library(leaflet)
library(leafem)
library(mapedit)
library(sf)
library(glue)

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
myhelptxt <- function(x) {
  helpText(HTML(glue('<i class="fas fa-info-circle" aria-hidden="true"></i> 
  {x}')))
}


hspace <- function(n) HTML(paste(rep("&nbsp;", n), collapse = ""))

xtc_elmt <- function(x, y) lapply(x, `[`, y)


## Check details

check_name <- function(x) {
  if (x != "") TRUE else FALSE
}

check_email <- function(x) {
  if (x != "") TRUE else FALSE
}

info_valid <- function(x, ok = TRUE) {
  cls <- ifelse(ok, "valid", "invalid")
  renderUI(HTML(glue("<span class={cls}>{shiny::icon('info')} {x}</span>")))
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
