renderReport <- function(x, fl = "report", dir = "output/doc") {
  msgInfo("Generating report", x)
  out <- tryCatch({
    rmarkdown::render(x, 
      output_format = "all", 
      output_dir = dir, 
      output_file = fl, 
      quiet = TRUE)
    paste(shiny::icon("info"), x, "has been successfully rendered (see, ", 
    paste0(dir, "/", fl, "*)."))
  }, 
  error = function(x) paste(shiny::icon("info"), x)
  )
  shiny::HTML(out)
  # check ohow to do that properly (err)
}