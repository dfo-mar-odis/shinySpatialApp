fullReportUI <- function(id) {
  ns <- NS(id)
  tagList(

    myhelptxt("This tab allows you to customize and generate your report."),

    tabsetPanel(

      tabPanel(
        "Select sections",
        myhelptxt("This tab allows you to select the sections you wish to include to your report."),
        checkboxGroupInput(
          ns("species"),
          label = "Species",
          choiceNames = c(
            "National Aquatic Species at Risk Program",
            "Species",
            "Aquatic Invasive Species"
          ),
          choiceValues = c(
            "species/NSARP_dist_crithab.Rmd",
            "species/species.Rmd",
            "species/aquatic_invasive.Rmd"
          ),
          selected = c(
            "species/NSARP_dist_crithab.Rmd",
            "species/species.Rmd",
            "species/aquatic_invasive.Rmd"
          )
        ),
        checkboxGroupInput(
          ns("context"),
          label = "Context",
          choiceNames = c(
            "Areas designated for spatial planning",
            "Habitat"
          ),
          choiceValues = c(
            "context/spatial_planning.Rmd",
            "context/habitat.Rmd"
          ),
          selected = c(
            "context/spatial_planning.Rmd",
            "context/habitat.Rmd"
          )
        ),
        checkboxGroupInput(
          ns("human_threats"),
          label = "Human threats",
          choiceNames = c(
            "Fishing",
            "Shipping",
            "Miscellaneous",
            "Cumulative impact mapping"
          ),
          choiceValues = c(
            "human_threats/fishing.Rmd",
            "human_threats/shipping.Rmd",
            "human_threats/miscellaneous.Rmd",
            "human_threats/cumulative.Rmd"
          ),
        ),
      ),

      tabPanel(
        "Generate report",
        myhelptxt("This tab allows you to generate your report"),
        radioButtons(
          ns("lang"),
          label = "Select target language(s) for report",
          choiceNames = c("English", "French"),
          choiceValues = c("EN", "FR"),
          selected = "EN",
          inline = TRUE
        ),
        textAreaInput(
          ns("u_text"),
          label = "Subtitle",
          value = "Synthesis prepared by the Reproducible Reporting Team, steering committee and advisors in Maritimes Region."
        ),
        textAreaInput(ns("u_comments"), label = "Comments", value = ""),
        textInput(
          ns("report_name"),
          label = "Report filename (optional, do not specify the file extension)",
          value = ""
        ),
        hr(),
        actionButton(
          ns("generate_rmd"), 
          "Generate report", 
          icon("book")
        ),
        hspace(2),
        uiOutput(
          ns("render_success"), 
          inline = TRUE
        ),
        br(),
        br(),
        downloadButton(ns("outputs_dl"), "Download outputs"),
        downloadButton(ns("report_dl"), "Download PDF")
      )
    )

  )
}



fullReportServer <- function(id, geoms, preview, u_details) {

  moduleServer(
    id,
    function(input, output, session) {

      shinyjs::hide(id = "outputs_dl")
      shinyjs::hide(id = "report_dl")

      observeEvent(input$generate_rmd, {
        
        if (!u_details$consent) {
          output$render_success <- info_valid("Please abide by terms and conditions in 'User' tab.", FALSE)
        } else {
          shinyjs::hide(id = "outputs_dl")
          shinyjs::hide(id = "report_dl")
          # Need to change the HTML to trigger the reactive expression.
          # A simple way is to set it to nothing at the beginning of the process
          # so the internet browser understands that the page is obsolete.
          preview$full_html <- ""
          id <- showNotification(
            "Rendering HTML preview ...", 
            duration = NULL, 
            closeButton = FALSE
          )
          # 
          chk <- render_full_report(
            geoms = geoms$final,
            lang = input$lang,
            u_name = u_details$name, 
            u_email = u_details$email,
            u_text = input$u_text,
            u_comments = input$u_comments,
            parts = c(
              input$species, 
              input$context, 
              input$human_threats
            ),
            fl = input$report_name
          )
          removeNotification(id)

          if (chk$ok) {
            output$render_success <- info_valid(chk$msg, chk$ok)
            preview$full_html <- chk$html
            
            # Download output 
            output$outputs_dl <- downloadHandler(
              filename = "output.zip",
              content = function(file) zip(file, "./output")
            )
            showNotification("Success", type = "message")
            shinyjs::show(id = "outputs_dl")
            shinyjs::show(id = "report_dl")
            
            # see https://mastering-shiny.org/action-transfer.html
            output$report_dl <- downloadHandler(
              filename = function() {
                switch_ext(basename(preview$full_html), "pdf")
              },
              content = function(file) {      
                id <- showNotification(
                  "Rendering PDF...", 
                  duration = NULL, 
                  closeButton = FALSE
                )
                on.exit(removeNotification(id), add = TRUE)
                fs::file_copy(pagedown::chrome_print(preview$full_html), file)
              })

          } else {
            showNotification("Abort rendering", type = "error")
            preview$full_html <- "www/wrong.html"
            output$render_success <- info_valid(chk$msg, FALSE)
            print(preview$full_html)
            shinyjs::hide(id = "outputs_dl")
            shinyjs::hide(id = "report_dl")
          }
        }
      })
    }
  )
}