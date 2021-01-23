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
  
  if (out) {
    # this is done to generate a html preview 
    # TODO test whether such html was generated 
    # clear HTML FIRST
    preview_html <- switch_ext(basename(x), "html")
    print(preview_html)
    rmarkdown::render(flrmd, 
      output_format = "html_document", 
      output_dir = "www", 
      quiet = TRUE)
  } else preview_html <- NULL
  
  
  
  list(ok = out, fl = flrmd, html = preview_html)
}