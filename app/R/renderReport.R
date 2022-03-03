#' Generate the Rmd reproducible full report
#'
#' @param geoms sf object selected.
#' @param lang target language, "EN"/"FR"
#' @param fl file name (optional).
#' @param species human_threats, context different parts to be added.
#' @param u_name, u_email, u_text, u_comments User details.
#' @param dir_out output directory.
#' @param dir_tpl path to template directory.
#'
#'
#' Returns list of values: 
#'   msg: info message (Successfully rendered/ Issue Rendering)
#'   ok: Boolean indicating sucess or not
#'   html: html of report
#'   dir_out: directory where report is saved
#'
#
render_full_report <- function(geoms, lang, species, human_threats, context,
  u_name, u_email, u_text, u_comments, dir_out = "output", fl = "",
  dir_tpl = here::here("app/templates"), keep.origin = FALSE) {
    
  # clear output
  clear_output()
 
  # Check and save geom 
  msgInfo("Saving geoms")
  if (is.null(geoms)) {
    msg <- "Please define areas of interest"
    return(list(msg = msg, ok = FALSE, html = "empty_report.html"))
  } else {
    flge <- save_geom(geoms, dir_out)
  }
  
  # Basename of report
  fl <- ifelse(
    fl == "",
    glue("report_SMR_{lang}.Rmd"),
    glue("{gsub(' ', '-', fl)}_{lang}.Rmd")
  )
  
  #
  msgInfo(glue("Creating report ({fl})"))
  #
  
  # fill out .Rmd file before rendering
  main <- "generic_intro.Rmd"
  template <- glue_path(dir_tpl, lang, main)
  # generic_intro.Rmd.origin contains paths to sections to be added 
  # in code chunks (child option)
  origin <- glue(add_lang_to_file(main, lang), ".origin")
  # full Rmd ready (txt within Rmd no longer in code chunks)
  rmd_ready <- glue_path(dir_out, add_lang_to_file(fl, lang))

  data_all <- list(
    path_to_geoms = flge$relrmd,
    add_species_parts = add_sections(
      species, lang, dir_tpl, "species"
    ),
    add_human_threats_parts = add_sections(
      human_threats, lang, dir_tpl, "human_threats"
    ),
    add_context_parts = add_sections(context, lang, dir_tpl, "context"),
    u_name = u_name,
    u_email = u_email,
    u_text = u_text,
    u_comments = u_comments
  )  
  # What does this do?
  #readLines(template)
  # creates .Rmd.origin
  # writeLines(whisker::whisker.render(readLines(template), data_all), origin)
  writeLines(whisker::whisker.render(readLines(template), data_all), rmd_ready)
  # creates .Rmd
  #knitr::knit(origin, rmd_ready, quiet = FALSE)
  # remove .Rmd.origin
  #if (!keep.origin) unlink(origin)
  # move figure folder
  #if (fs::dir_exists("figs")) {
  #  fs::dir_copy("figs", "output/figs", overwrite = TRUE)
  #  fs::dir_delete("figs")
  #}
  
  
  msgInfo("Creating HTML")  
  # Rendering to HTML 
  ok <- tryCatch({
      rmarkdown::render(rmd_ready, 
        quiet = FALSE)
        TRUE
      }, 
      error = function(x) FALSE
    )
  # 
  if (ok) {
    # this is done to generate a html preview (see "report" tab)
    html <- switch_ext(basename(rmd_ready), "html")
    preview_html <- glue_path("www", "reports", html)
    msgInfo("Copying HTML")
    jj <- file.copy(
      glue_path(dir_out, html), 
      preview_html
    )
    msg <- "Successfully rendered."
  } else {
    msg <- "Issue while rendering"
    preview_html <- glue_path("www", "empty_report.html")
  }
  # 
  list(msg = msg, ok = ok, html = preview_html, dir_out = dir_out)
}


add_sections <- function(files, lang, dir_in, dir_part) {
  if (length(files)) {
    # cannot use a vector of length > 1 so lapply() is called
    fls <- lapply(files, function(x) glue_path(dir_in, lang, dir_part, x))
    fls <- paste0("'", unlist(fls), "'")
    glue("```{{r, child = c({glue_collapse(fls, sep = ', ')})}}\n```")

  } else NULL
}


save_geom <- function(geoms, dir_out, flnm ="geoms_slc.geojson") {
  pth <- glue("{dir_out}/{flnm}")
  sf::st_write(geoms, pth, delete_dsn = TRUE, quiet = TRUE)
  list(relroot = pth, relrmd = pth)
}

