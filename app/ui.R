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
    titlePanel(""),

    # Sidebar with a ui for grabbing mapedit data
    sidebarLayout(

        sidebarPanel( 
          h2(div(img(src="img/bio_logo.jpg", height = 80), "Spatial Shiny App")),
          
          tabsetPanel(
            
            # IDENTIFICATION
            tabPanel(
              "Identify", 
              icon = icon("user-check"),
              helpText("This tab allows you to identify yourself."),
              textInput("user_name", label = "Enter your name", value = "Lorem ipsum"),
              textInput("user_email", label = "Enter your email", value = "lipsum@dfo-mpo.gc.ca"),
              textAreaInput("user_notes", label = "Provide the reason/rationale for generating the report", value = ""),
              checkboxInput("user_consent", label = "By checking this box, you abide to ..."),
              br(),
              actionButton("get_user_details", 'Valid details', icon = icon("pencil")),
              hspace(2),
              uiOutput("valid_details", inline = TRUE),
              uiOutput("invalid_details", inline = TRUE),
            ),
            
            
            
            # LAYER CREATION 
            tabPanel(
            "Create", 
            icon = icon("pencil"),
            helpText("This tab allows you to create and save layers."),
            tabsetPanel(
              tabPanel(
                "Bounding box",
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
                actionButton('add_bbox', 'Add to map', icon = icon("pencil")),
                actionButton('save_bbox', 'Save', icon = icon("download"))
              ),
              tabPanel(
                "Draw from map",
                h4("Create geom from map (geojson)"),
                textInput("name_geom", label = "Enter layer name (also used as file name)", value = "new_geom"),
                actionButton('save_from_map', 'Save from map', icon = icon("download")),
                hspace(2),
                uiOutput("created_from_map", inline = TRUE),
                uiOutput("created_from_map_not", inline = TRUE)
              ),
              tabPanel(
                "Input Shapefile",
                h4("Use your own shapefile"),
                fileInput("import_shapefile", "Choose a file", accept = c(".shp", ".geojson")),
                actionButton('shp_to_map', 'Add to map', icon = icon("pencil")),
                actionButton('save_shp', 'Save', icon = icon("download"))
              )
            )
          ),
          
          
          # LAYER ADDITION 
          # tabPanel(
          #   "Crete",
          #   tabsetPanel(
          #     tabPanel(
          #       "Identify",
          #     ),
          #     tabPanel(
          #       "Identify2",
          #     )
          #   )
          # ),
          # tabPanel(
          #   "Add", 
          #   icon = icon("database"),
          #   helpText("This tab allows you to add layers on map."),
          #   checkboxGroupInput("data_src", 
          #                     label = "Select the data you need", 
          #                     choiceNames = tb_ref$layer,
          #                     choiceValues = tb_ref$id),
          # 
          #   h4("Add a spatial file"),
          #   fileInput("spa_ext", "Choose a file",
          #   accept = c(".kml", ".tiff", ".shp", ".geojson")
          #   )
          # ),
          
          
            
            # SPATIAL OPERATION(S) 
            tabPanel(
              "Manipulate", 
              icon = icon("cog"),
              helpText("This tab allows you to specify the spatial operation(s) to be performed."),
              
              tabsetPanel(
                tabPanel(
                  "Data source",
                  br(),
                  checkboxGroupInput("data_src", 
                                      label = "Select the data you need", 
                                      choiceNames = tb_ref$layer,
                                      choiceValues = tb_ref$id),
                ),
                tabPanel(
                  "Input Area(s)",
                  br(),
                  checkboxGroupInput("check_input_areas", "Select area", 
                    c("none"))
                ),
                tabPanel(
                  "Perform operation(s)",
                  br(),
                  selectInput("oper_slc", "Select one available operation", 
                           choices = ls_oper, 
                           selected = "none"),           
                  numericInput("geom_buffer", label = "Optional buffer (in meters)", value = "0", min = 0),
              
                  textInput("name_output", label = "Enter output name (filename)", value = "output_01"),
                  actionButton("oper_doit", "Perform operation", icon("cog"))
                )
              )
              
            ),
            
            # REPORT
            tabPanel(
              "Report", 
              icon = icon("book"),
              helpText("This tab allows you to generate a report."),
              selectInput("int_rmd", "Select one available template", 
                       choices = c(list("none" = "none"), rmd_list), 
                       selected = "none"),
              fileInput("ext_rmd", "Choose an R Markdown file (.Rmd)", 
              accept = c(".Rmd", ".rmd")),
              textInput("report_name", label = "Report name (optional)", value = ""),
              HTML("<h4>File selected: "),
              textOutput("file_report", inline = TRUE),
              HTML("</h4>"),
              hr(),
              actionButton("generate_rmd", "Generate report", icon("book")),
              hspace(2),
              uiOutput("render_success", inline = TRUE)
            )
          ),
            
            
          br(),
          br(),
          hr(),
          
          HTML(
          paste0(
            "<h5 align = 'center'>This shiny app was built with the ", 
            a("R package shiny", href = "https://shiny.rstudio.com/"), 
            ", source code available on ")
          ),
          a(icon("github"), href = "https://github.com/inSilecoInc/shinySpatialApp"),
          HTML("</h5>")
            
        ),
        

          


        # add map
        mainPanel(
          
          
          tabsetPanel(
            
            # MAP
            tabPanel(
              "Map",
              icon = icon("map"),
              mapedit::editModUI("map")
            ),
            
            # REPORT
            tabPanel(
              "Report",
              icon = icon("file-alt"),
              htmlOutput("report_html"),
              br()
            )
        ),
      )
      
    )
    
)

