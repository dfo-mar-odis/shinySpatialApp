renderReport <- function(x, fl = "", dir = "output/doc") {
  msgInfo("Generating report", x)
  if (fl == "") {
    fl <- NULL 
  } else {
    # nasty trick due to current behavior   
    fl <- rep(20)
  }
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