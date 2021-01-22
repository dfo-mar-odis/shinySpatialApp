renderReport <- function(x, fl = "", data, dir = "output/doc") {
  msgInfo("Generating report", x)

  if (fl == "") {
    fl <- NULL 
  } else {
    # nasty trick due to current behavior of rmarkdown
    fl <- rep(fl, 20)
  }
  
  flrmd <- glue("{dir}/{basename(x)}")
  
  # fill out rmd file 
  template <- readLines(x)
  writeLines(whisker::whisker.render(template, data), flrmd)
  
  out <- tryCatch({
    rmarkdown::render(flrmd, 
      output_format = "all", 
      output_dir = dir, 
      output_file = fl, 
      quiet = TRUE)
    TRUE
  }, 
  error = function(x) FALSE
  )
  list(ok = out, fl = flrmd)
}