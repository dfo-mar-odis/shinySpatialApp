# load packages and R scripts
lapply(list.files("R/", pattern = ".[Rr]$", full.names = TRUE), source)

# create table dsn/layer

lay <- lapply(fl, function(x) sf::st_layers(x)$name)
tb_ref <- data.frame(
         id = seq_len(sum(lengths(lay))),
         dsn = rep(unlist(fl), lengths(lay)),
         layer = unlist(lay)
       )


ui <- fluidPage(

  useShinyjs(),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "extra.css")
  ),

  # Application title
  titlePanel("", "BIO Spatial Shiny App"),

  #
  sidebarLayout(

    sidebarPanel(
      # Title
      h2(div(img(src="img/bio_logo.jpg", height = 78), "Spatial Shiny App")),

      # Panels
      tabsetPanel(
        id = "active_panel",

        # IDENTIFICATION
        tabPanel(
          "User",
          icon = icon("user-check"),
          myhelptxt("This tab allows you to identify yourself, detail the reason(s)
          why you are generating the report and abide to terms and conditions."),
          # input
          textInput("u_name", label = "Enter your name", value = "Lorem ipsum"),
          textInput("u_email", label = "Enter your email", value = "lipsum@dfo-mpo.gc.ca"),
          textAreaInput("u_notes", label = "Provide the reason/rationale for generating the report", value = ""),
          # terms and conditions
          checkboxGroupInput("u_consent", label = "Terms and conditions",
          choiceNames = list(
            HTML("I understand this report is for the <a href = 'https://www.dfo-mpo.gc.ca/index-eng.htm' target='_blank'>Department of Fisheries and Oceans Canada (DFO)</a> internal use only, and it shall not be shared with users outside DFO."),
            HTML("I will read all the caveats, disclaimers and uncertainty outlined in each section of the report."),
            HTML("I will abide to all policies and directives of the Government of Canada, including, and not restricted to, <a href = 'https://www.canada.ca/en/government/publicservice/values.html' target='_blank'>values and ethics of the public service</a>.")
            ),
            choiceValues = list(1, 2, 3)
          ),
          br(),
          actionButton("get_u_details", 'Validate', icon =icon("pencil")),
          hspace(2),
          uiOutput("valid_details", inline = TRUE)
        ),

        # LAYER CREATION
        tabPanel(
          "Geometries",
          icon = icon("pencil"),
          myhelptxt("This tab allows you to create or import areas of interest using one of the three tabs below."),

          # Three selections panels
          tabsetPanel(

            tabPanel(
              "Bounding box",
              myhelptxt("Use the fields below to create a bounding box."),
              h4("Enter coordinates of the bounding box"),
              div(style="display: inline-block;vertical-align:top;",
                numericInput("bbox_xmin", label = "Xmin", value = "0", min = -180, max = 180)),
              div(style="display: inline-block;vertical-align:top;",
                numericInput("bbox_xmax", label = "Xmax", value = "0", min = -180, max = 180)),
              br(),
              div(style="display: inline-block;vertical-align:top;",
                numericInput("bbox_ymin", label = "Ymin", value = "0", min = -90, max = 90)),
              div(style="display: inline-block;vertical-align:top;",
                numericInput("bbox_ymax", label = "Ymax", value = "0", min = -90, max = 90)),
              br(),
              numericInput("bbox_crs", label = HTML("Spatial projection (see EPSG code: <a href='https://epsg.io/' target='_blank'>https://epsg.io/</a>)"), value = "4326"),
              numericInput("bbox_buffer", label = "Optional buffer (m)", value = "0", min = 0),
              br(),
              myhelptxt("Save bounding box. Note that geometries will not be automatically added to the map. Go to the <code>Check</code> tab to add geometries."),
              actionButton('save_bbox', 'Save', icon = icon("download"))
            ),

            tabPanel(
              "Individual point",
              myhelptxt("Use the fields below to create a bounding box centered on an individual point with a buffer."),
              h4("Enter coordinates of the bounding box"),
              div(style="display: inline-block;vertical-align:top;",
                numericInput("pt_x", label = "X", value = "0", min = -180, max = 180)),
              div(style="display: inline-block;vertical-align:top;",
                numericInput("pt_y", label = "Y", value = "0", min = -180, max = 180)),
              br(),
              numericInput("pt_crs", label = HTML("Spatial projection (see EPSG code: <a href='https://epsg.io/' target='_blank'>https://epsg.io/</a>)"), value = "4326"),
              numericInput("pt_buffer", label = "Optional buffer (m)", value = "0", min = 0),
              br(),
              myhelptxt("Save bounding box. Note that geometries will not be automatically added to the map. Go to the <code>Check</code> tab to add geometries."),
              actionButton('save_pt', 'Save', icon = icon("download"))
            ),

            tabPanel(
              "Draw from map",
              myhelptxt("Use the interactive map on the right side to create geometries (points, lines or polygons)."),
              h4("Create geometries from map"),
              numericInput("from_map_buffer", label = "Optional buffer (m)", value = "0", min = 0),
              myhelptxt("Save created geometry for use when generating report."),
              actionButton('save_from_map', 'Save from map', icon = icon("download")),
              hspace(2),
              uiOutput("created_from_map", inline = TRUE),
              uiOutput("from_map_not", inline = TRUE)
            ),

            tabPanel(
              "Import files",
              myhelptxt("Import your own file. The file will be read by
              <a href='https://www.rdocumentation.org/packages/sf/versions/0.2-2/topics/st_read' target='_blank'>
              <code>sf::st_read()</code></a>
              which supports a vast variety of vector formats. Note that for ESRI Shapfiles, several files need to be uploaded, that is why the file selection below allow for multiple files to be uploaded at once."),
              h4("Use your own shapefile"),
              fileInput("import_shapefile", "Choose a file", multiple = TRUE),
              numericInput("import_buffer", label = "Optional buffer (m)", value = "0", min = 0),
              actionButton('save_import', 'Save', icon = icon("download"))
              )
            ),

            br(),
            uiOutput("nb_geoms")
          ),


        # VALID GEOMS
        tabPanel(
          "Check",
          icon = icon("check-square"),
          myhelptxt("This tab allows you to validate geometries you wish to use to generate the report and, if desired, add a buffer to selected geometries."),
          checkboxGroupInput("check_input_areas", choiceNames = "Input placeholder1", choiceValues = 1, c("none")),
          #
          actionButton('add_geoms_to_map', 'Add to map', icon = icon("pencil")),
          actionButton('valid_geoms', "Validate", icon = icon("check")),
          actionButton('clear_map', "Clear map", icon = icon("trash-alt")),
          br(),
          br(),
          uiOutput("nb_geoms_selected")
        ),



        # REPORT
        tabPanel(
          "Report",
          icon = icon("book"),
          myhelptxt("This tab allows you to customize and generate your report."),

          tabsetPanel(

            tabPanel(
              "Select sections",
              myhelptxt("This tab allows you to select the sections you wish to include to your report."),
              checkboxGroupInput("main_sections",
                label = "While numerous species have been identified in this region, only those listed by SARA, or assessed by COSEWIC and Wild species listings are summarized in this section:",
                choiceNames = c(
                  "National Aquatic Species at Risk",
                  "Fish and Invertebrates",
                  "Sea turtles",
                  "Cetaceans"
                ),
                selected = 1:4,
                choiceValues = 1:4,
              ),
              checkboxGroupInput("extra_sections",
                label = "The following selection will include additional information, and species not listed by SARA, or assessed by COSEWIC and Wild species:",
                choiceNames = c(
                  "Ecologically or Biologically Significant Areas",
                  "Additional species"
                ),
                choiceValues = 1:2,
              ),

            ),

            tabPanel(
              "Generate report",
              myhelptxt("This tab allows you to generate your report"),
              checkboxGroupInput("report_lang",
                label = "Select target language(s) for report",
                choiceNames = c("English", "French"),
                choiceValues = c("EN", "FR"),
                selected = "EN",
                inline = TRUE
              ),
              textAreaInput(
                "u_text",
                label = "Subtitle",
                value = "Synthesis prepared by the Reproducible Reporting Team, steering committee and advisors."
              ),
              textAreaInput("u_comments", label = "Comments", value = ""),
              textInput(
                "report_name",
                label = "Report filename (optional, do not specify the file extension)",
                value = ""
              ),
              hr(),
              actionButton("generate_rmd", "Generate report", icon("book")),
              hspace(2),
              uiOutput("render_success", inline = TRUE),
              br(),
              br(),
              downloadButton("dl_outputs", "Download outputs")
            )
            )
          )
          ),

          br(),
          br(),
          hr(),


          HTML(
          paste0(
            "<div id='footer_left'><a href='https://insilecoinc.github.io/' target='_blank'><img src='img/insileco_logo256.png' alt='inSileco logo' width = '100%'/></a></div><div id='footer_right'><h5>This shiny app was built by <a href='https://insilecoinc.github.io/' target='_blank'>inSileco</a> with the ",
            a("R package shiny", href = "https://shiny.rstudio.com/", target = "_blank"),
            ", the source code available on ")
          ),
          a(icon("github"), href = "https://github.com/inSilecoInc/shinySpatialApp", target = "_blank"),
          HTML(".</h5></div>")

        ),


    # RIGHT PANEL
    # add map
    mainPanel(

      tabsetPanel(
        id = "map_or_report",
        # MAP
        tabPanel(
          "Map",
          icon = icon("map"),
          mapedit::editModUI("map")
        ),
        # REPORT
        tabPanel(
          "Report",
          icon = icon("book"),
          htmlOutput("report_html")
        )
      ),
    )


  )

)
