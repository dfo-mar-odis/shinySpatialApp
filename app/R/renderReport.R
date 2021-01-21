renderReport <- function(x, fl = "", dir = "output/doc", data) {
  msgInfo("Generating report", x)

  
  if (fl == "") {
    fl <- NULL 
  } else {
    # nasty trick due to current behavior of makrdown
    fl <- rep(20)
  }
  
  # fill out rmd file 
  
  template <- readLines(x)
  writeLines(whisker::whisker.render(x, data), paste0(dir, basename(x)))
  
  
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
  # check how to do that properly (err)
}