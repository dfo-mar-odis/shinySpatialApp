# Functions to generate the Rmd report
#' @param data data 
#' @param input input list (from app)
#' @param geoms sf object selected (from app)
#' @param fl file name (optional)
#' @param dir_out output directory
#' @param dir_in input directory
# 
renderReport <- function(data, input, geoms, fl = NULL, dir_out = "output", 
  dir_in = "Rmd") {
  
  # dev help  
  msgInfo("Generating report")
  
  # rm all html 
  clear_www_html()
  clear_zip()
  # report not generated yet
  ok <- FALSE
  # lang of the report 
  lang <- input$report_lang
  
  # check and save geom 
  msgInfo("Saving geoms")
  if (is.null(geoms)) {
    msg <- "Please define areas of interest"
    return(list(msg = msg, ok = ok, fl = NULL, html = NULL))
  } else {
    flge <- save_geom(geoms)
  }
  
  
  msgInfo("Report in ")
  # for (i in seq_along(lang)) {
  # 
  # }
  
  # nasty trick due to current rmarkdown behavior
  if (fl != "" & !is.null(fl)) {
     fl <- rep(fl, 20)
  } else fl <- NULL
  x <- glue("{dir_in}/report_pt1_generic_intro_{lang}.Rmd")

  
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
    input,
    path_to_geoms = flge$relrmd,
    add_main_sections = add_sections(s_main, dir_in, dir_out),
    add_ebsa_section = add_sections(s_ebsa, dir_in, dir_out),
    add_appendix = add_sections(s_appendix, dir_in, dir_out)
  )  
  writeLines(whisker::whisker.render(template, data_all), flrmd)

  # First rendering
  msg <- tryCatch({
    rmarkdown::render(flrmd, 
      output_format = "all", 
      output_dir = dir_out, 
      output_file = fl, 
      quiet = TRUE)
  }, 
    error = function(x) FALSE
  )
  

  if (!isFALSE(msg)) {
    # this is done to generate a html preview (see "report" tab)
    preview_html <- switch_ext(basename(x), "html")
    gnr_html <- switch_ext(flrmd, "html")
    # do not render again if html has already been generated
    if (file.exists(gnr_html)) {
      file.copy(gnr_html, glue("www/{preview_html}"))
    } else {
      rmarkdown::render(flrmd, 
        output_format = "html_document", 
        output_dir = "www", 
        quiet = TRUE)
    }
    msg <- "Report successfully generated."
    ok <- TRUE 
  } else {
    msg_error <- "Issue while rendering"
    flrmd <- preview_html <- NULL
  }

  list(msg = msg, ok = ok, fl = flrmd, html = preview_html)
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

save_geom <- function(geoms, dir_out = "output", flnm ="geoms_slc.geojson") {
  st_write(geoms, glue("{dir_out}/{flnm}"), delete_dsn = TRUE)
  list(relroot = glue("{dir_out}/{flnm}"), relrmd = flnm)
}