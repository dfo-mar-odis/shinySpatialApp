#' @param data data 
#' @param input input list (shiny)
#' @param fl file name (optional)
#' @param dir_out output directory
#' @param dir_in input directory
# 
renderReport <- function(data, input, fl = NULL, dir_out = "output/doc", 
  dir_in = "Rmd") {
    
  msgInfo("Generating report")
  #
  clear_www_html()
  
  # 
  lang <- input$report_lang
  # nasty trick due to current rmarkdown behavior
  if (fl != "" & !is.null(fl)) {
     fl <- rep(fl, 20)
  } else fl <- NULL
  x <- glue("{dir_in}/report_pt1_generic_intro_{lang}.Rmd")
  
  # Save data as rds 
  # dtrmd <- switch_ext(glue("data_{basename(x)}"), "rds")
  # saveRDS(data, glue("{dir_out}/{dtrmd}"))

  
  # Section(s) to be added
  s_main <- main_parts(input$main_sections, lang)
  s_ebsa <- s_appendix <- NULL
  if (!is.null(input$extra_sections)) {
    s_ebsa <- ebsa_part(any(input$extra_sections == 1), lang)
    s_appendix <- appendix_part(any(input$extra_sections == 2), lang)
  }

  # fill out .Rmd file before rendering it
  flrmd <- glue("{dir_out}/{basename(x)}")
  template <- readLines(x)
  data_all <- c(
    data, 
    input,
    add_main_sections = add_sections(s_main, dir_in, dir_out),
    add_ebsa_section = add_sections(s_ebsa, dir_in, dir_out),
    add_appendix = add_sections(s_appendix, dir_in, dir_out)
  )  
  writeLines(whisker::whisker.render(template, data_all), flrmd)

  # First rendering
  # rmarkdown::render(flrmd, 
  #   output_format = "all", 
  #   output_dir = dir_out, 
  #   output_file = fl, 
  #   quiet = TRUE)
  out <- tryCatch({
    rmarkdown::render(flrmd, 
      output_format = "all", 
      output_dir = dir_out, 
      output_file = fl, 
      quiet = TRUE)
    TRUE
  }, 
    error = function(x) FALSE
  )
  
  # flrmd <- glue("{dir_out}/{basename(x)}")

  if (out) {
    # this is done to generate a html preview (see "report" tab)
    preview_html <- switch_ext(basename(x), "html")
    gnr_html <- switch_ext(flrmd, "html")
    # do not render again if html has already been generated
    if (file.exists(gnr_html)) {
      file.copy(gnr_html, glue("www/{preview_html}"))
    } else {
      print(flrmd)
      rmarkdown::render(flrmd, 
        output_format = "html_document", 
        output_dir = "www", 
        quiet = TRUE)
    }
  } else preview_html <- NULL
      
  list(ok = out, fl = flrmd, html = preview_html)
}


# add rmd code chunk to add section (using child documents)
add_sections <- function(flnms = NULL, dir_in, dir_out) {
  if (!is.null(flnms)) {
    vc_nm <- paste0("'", flnms, "'") 
    # copy/paste file
    file.copy(paste0(dir_in, "/", flnms), dir_out)
    #
    glue("```{{r, child = c({glue_collapse(vc_nm, sep = ', ')})}}\n```")
  } else flnms
}


main_parts <- function(x, lang = c("EN", "FR")) {
  lang <- match.arg(lang)
  vc <- c(
    "report_pt2_SAR_dist_crithab",
    "report_pt3_fish_inverts",
    "report_pt4_seaturtles",
    "report_pt5_cetaceans"
  )[as.numeric(x)]
  paste0(vc, "_", lang, ".Rmd")
}

ebsa_part <- function(x, lang = c("EN", "FR")) {
  lang <- match.arg(lang)
  ifelse(x, glue("report_pt6_EBSA_{lang}.Rmd"), NULL)
}

appendix_part <- function(x, lang = c("EN", "FR")) {
  lang <- match.arg(lang)
  ifelse(x, glue("report_pt8_Appendix_{lang}.Rmd"), NULL)
}

