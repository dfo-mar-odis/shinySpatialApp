customReportUI <- function(id) {
  ns <- NS(id)
  tagList(
    myhelptxt("This tab allows you to create customized reports."),
    radioButtons(
      ns("lang"),
      label = "Select target language(s) for report",
      choiceNames = c("English", "French"),
      choiceValues = c("EN", "FR"),
      selected = "EN",
      inline = TRUE
    ),
    radioButtons(
      ns("type"), 
      label = "Select report type", 
      choiceNames = c("Aquaculture Siting", "Coastal Monitoring", "Conservation Planning", "Environmental Response"),
      choiceValues = c("aqua.Rmd", "coastal.Rmd", "conservation.Rmd", "envresponse.Rmd"),
      selected = "aqua.Rmd"
    ),
    textInput(
      ns("filename"),
      label = "Report filename (optional, do not specify the file extension)",
      value = ""
    ),
    hr(),
    actionButton(ns("report_gen"), "Generate report", icon("book")),
    downloadButton(ns("report_dl"), "Download PDF")
  )
}


renderCustomReport <- function(id, preview, u_name, u_email, u_consent) {
  moduleServer(
    id,
    function(input, output, session) {
      
      shinyjs::hide(id = "report_dl")
      
      observeEvent(input$report_gen, {
        print(u_consent)
        print(u_name)
        if (length(u_consent) != 3) {
          showNotification(
            "Please abide by terms and conditions in 'User' tab.", 
            type = "error"
          )
        } else {
          # template path
          tpl <- glue_path("templates", input$lang, "custom", input$type)
          # out path (add EN/FR)
          fln <- ifelse(
              input$filename == "", 
              glue(
                fs::path_ext_remove(input$type), 
                "_", tolower(input$lang), ".Rmd"
              ),
              glue(input$filename, "_", tolower(input$lang), ".Rmd")
            )
          out <- glue_path("www", "reports", fln)
          # render template 
          writeLines(
            whisker::whisker.render(
              readLines(tpl, warn = FALSE), 
              list(
                u_name = u_name,
                u_email = u_email,
                u_text = "Synthesis prepared by the Reproducible Reporting Team, steering committee and advisors in Maritimes Region."
              )
            ),  
            out
          )
          # render document
          showNotification("Rendering HTML", type = "message")
          preview$custom_html <- rmarkdown::render(out, "html_document")
          # unlink(out) # to remove rmd file
          shinyjs::show(id = "report_dl")
        }
      })
      
      # see https://mastering-shiny.org/action-transfer.html
      output$report_dl <- downloadHandler(
        filename = function() {
          switch_ext(basename(preview$custom_html), "pdf")
        },
        content = function(file) {      
          id <- showNotification(
            "Rendering PDF...", 
            duration = NULL, 
            closeButton = FALSE
          )
          on.exit(removeNotification(id), add = TRUE)
          fs::file_copy(pagedown::chrome_print(preview$custom_html), file)
        })
        
    }
  )
}
