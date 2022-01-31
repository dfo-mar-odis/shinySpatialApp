ui <- fluidPage(

  useShinyjs(),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "extra.css")
  ),

  # Application title
  titlePanel("", "Spatial Reproducible Reporting"),

  #
  sidebarLayout(

    sidebarPanel(
      # Title
      img(src="img/canada_c.png", height = 40),
      h3(tags$b("Spatial Reproducible Reporting")),
      # Panels
      tabsetPanel(
        id = "active_panel",

        # IDENTIFICATION
        tabPanel(
          "User",
          icon = icon("user-check"),
          myhelptxt("This tab allows you to identify yourself, detail the reason(s)
          why you are generating the report and abide by terms and conditions."),
          # input
          textInput("u_name", label = "Enter your name", value = "Rod Knee"),
          textInput("u_email", label = "Enter your email", value = "rod_knee@dfo-mpo.gc.ca"),
          textAreaInput("u_notes", label = "Provide the reason/rationale for generating the report", value = ""),
          # terms and conditions
          checkboxGroupInput("u_consent", label = "Terms and conditions",
          choiceNames = list(
            HTML("I understand this report is for the <a href = 'https://www.dfo-mpo.gc.ca/index-eng.htm' target='_blank'>Department of Fisheries and Oceans Canada (DFO)</a> internal use only, and it shall not be shared with users outside DFO."),
            HTML("I will read all the caveats, disclaimers and uncertainties outlined in each section of the report."),
            HTML("I will abide by all policies and directives of the Government of Canada, including, and not restricted to, <a href = 'https://www.canada.ca/en/government/publicservice/values.html' target='_blank'>values and ethics of the public service</a>.")
            ),
            choiceValues = list(1, 2, 3)
          ),
          br(),
          actionButton("get_u_details", 'Validate', icon =icon("pencil-alt")),
          hspace(2),
          uiOutput("valid_details", inline = TRUE)
        ),

        # LAYER CREATION
        tabPanel(
          "Geometries",
          icon = icon("pencil-alt"),
          myhelptxt("This tab allows you to create or import areas of interest using one of the three tabs below."),

          # Three selections panels
          tabsetPanel(
            
            tabPanel(
              "Bounding box",
              myhelptxt("Use the fields below to create a bounding box."),
              div(style="display: inline-block; vertical-align:top;",
                numericInput("bbox_xmin", label = "Longitude min", value = "-67.183", min = -180, max = 180)),
              div(style="display: inline-block; vertical-align:top;",
                numericInput("bbox_xmax", label = "Longitude max", value = "-66.183", min = -180, max = 180)),
              br(),
              div(style="display: inline-block; vertical-align:top;",
                numericInput("bbox_ymin", label = "Latitude min", value = "44.650", min = -90, max = 90)),
              div(style="display: inline-block; vertical-align:top;",
                numericInput("bbox_ymax", label = "Latitude max", value = "45.650", min = -90, max = 90)),
              br(),
              numericInput("bbox_buffer", label = "Optional buffer (m)", value = "10000", min = 0, max = 25000000),
              tags$details(
                  tags$summary("Advanced options", icon("cog")),
                  numericInput("bbox_crs", label = HTML("Spatial projection (see EPSG code: <a href='https://epsg.io/' target='_blank'>https://epsg.io/</a>)"), value = "4326")
                ),
              br(),
              myhelptxt("Save bounding box. Note that geometries will not be automatically added to the map. Go to the <code>Check</code> tab to add geometries."),
              actionButton('save_bbox', 'Save', icon = icon("download"))
            ),

            tabPanel(
              "Individual point",
              myhelptxt("Use the fields below to create a bounding box centered on an individual point with a buffer."),
              div(style="display: inline-block;vertical-align:top;",
                numericInput("pt_x", label = "Longitude", value = "0", min = -180, max = 180)),
              div(style="display: inline-block;vertical-align:top;",
                numericInput("pt_y", label = "Latitude", value = "0", min = -180, max = 180)),
              br(),
              numericInput("pt_buffer", label = "Optional buffer (m)", value = "100000", min = 0, max = 25000000),
              tags$details(
                  tags$summary("Advanced options", icon("cog")),
                  numericInput("pt_crs", label = HTML("Spatial projection (see EPSG code: <a href='https://epsg.io/' target='_blank'>https://epsg.io/</a>)"), value = "4326")
                ),
              br(),
              myhelptxt("Save bounding box. Note that geometries will not be automatically added to the map. Go to the <code>Check</code> tab to add geometries."),
              actionButton('save_pt', 'Save', icon = icon("download"))
            ),

            tabPanel(
              "Draw from map",
              myhelptxt("Use the interactive map on the right side to create geometries (points, lines or polygons)."),
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
            actionButton('add_geoms_to_map', 'Add to map', icon = icon("pencil-alt")),
            actionButton('valid_geoms', "Validate", icon = icon("check")),
            actionButton('clear_map', "Clear map", icon = icon("trash-alt")),
            br(),
            br(),
            uiOutput("nb_geoms_selected")
          ),

          # REPORTS  
          tabPanel(
            "Full report",
            icon = icon("book"),
            fullReportUI(id = "tmp_full_report")  
            ),
            
          tabPanel(
            "Custom report",
            icon = icon("book"),
            customReportUI(id = "custom_report")  
          )
          
        ),
        
        
        
        hr(),
        # FOOTER 
        HTML(
        paste0(
          "<div id='footer_left'><a href='https://insilecoinc.github.io/' target='_blank'><img src='img/insileco_logo256.png' alt='inSileco logo' width = '100%'/></a></div><div id='footer_right'><h5>This shiny app was built by <a href='https://insilecoinc.github.io/' target='_blank'>inSileco</a> with the ",
          a("R package shiny", href = "https://shiny.rstudio.com/", 
            target = "_blank"), ", the content of the report is being developed by DFO Maritimes Region, and the source code is available on ")
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
        # FULL REPORT
        tabPanel(
          "Full report",
          icon = icon("book"),
          htmlOutput("full_report_html")
        ),
        # CUSTOM REPORTS 
        tabPanel(
          "Custom report",
          icon = icon("book"),
          htmlOutput("custom_report_html")
        )
      ),
    )

  )

)
