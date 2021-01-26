renderReport <- function(x, fl = "", data, dir = "output/doc") {
  msgInfo("Generating report", x)

  if (fl == "") {
    fl <- NULL 
  } else {
    # nasty trick due to current behavior of RMarkdown
    fl <- rep(fl, 20)
  }
  
  # Save data as rds 
  dtrmd <- switch_ext(glue("data_{basename(x)}"), "rds")
  saveRDS(data, glue("{dir}/dtrmd)"))
  
  # fill out .Rmd file before rendering
  flrmd <- glue("{dir}/{basename(x)}")
  template <- readLines(x)
  writeLines(whisker::whisker.render(template, c(data, data_path = dtrmd)), 
    flrmd)
  
  # First rendering
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
    # if (file.exists(preview_html)) print("file_do_exist")
    print(preview_html)
    rmarkdown::render(flrmd, 
      output_format = "html_document", 
      output_dir = "www", 
      quiet = TRUE)
  } else preview_html <- NULL
    
  
  
  list(ok = out, fl = flrmd, html = preview_html)
}