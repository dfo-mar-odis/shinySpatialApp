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
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "extra.css")
  ),
  
    # Application title
    titlePanel(""),

    # Sidebar with a ui for grabbing mapedit data
    sidebarLayout(
        # ,
        sidebarPanel( 
          h2(div(img(src="img/bio_logo.jpg", height = 80), "Spatial Shiny App")),
          
          tabsetPanel(
            
            # Identification
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
              textOutput("valid_details", inline = TRUE),
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
            
            # LAYER CREATION 
            tabPanel(
            "Create", 
            icon = icon("pencil"),
            helpText("This tab allows you to create and save layers."),
            tabsetPanel(
              tabPanel(
                "Bounding box",
                h4("Enter coordinates of the bounding box"),
                numericInput("bbox_xmin", label = "Xmin", value = "0", min = -180, max = 180),
                numericInput("bbox_xmax", label = "Xmax", value = "0", min = -180, max = 180),
                numericInput("bbox_ymin", label = "Ymin", value = "0", min = -90, max = 90),
                numericInput("bbox_ymax", label = "Ymax", value = "0", min = -90, max = 90),
                br(),
                actionButton('save', 'Add to map', icon = icon("pencil"))
              ),
              tabPanel(
                "Draw from map",
                h4("Create geom from map (geojson)"),
                textInput("name_geom", label = "Enter layer name (also used as file name)", value = "new_geom"),
                numericInput("geom_buffer", label = "Optional buffer (in meters)", value = "0", min = 0),
                actionButton('save', 'Create from Map', icon = icon("pencil")),
              ),
              tabPanel(
                "Input Shapefile",
                h4("Use your own shapefile"),
                fileInput("spa_ext", "Choose a file",
                  accept = c(".shp", ".geojson")
                  )
              )
            )
            
            

          ),
            
            # SPATIAL OPERATION(S) 
            tabPanel(
              "Manipulate", 
              icon = icon("cog"),
              helpText("This tab allows you performs spatial operation."),
              selectInput("oper_slc", "Select one available operation", 
                       choices = ls_oper, 
                       selected = "none"),
          
              textInput("name_output", label = "Enter output name (filename)", value = "output_01"),
              actionButton("oper_doit", "Perform operation", icon("cog"))
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
              htmlOutput("render_success"),
            )
          ),
            
            
        ),


        # add map
        mainPanel(
            mapedit::editModUI("map")
        )

    ), 
    
    hr(),
    
    HTML(
    paste0(
      "<h4 align = 'center'>This shiny app was built with the ", 
      a("R package shiny", href = "https://shiny.rstudio.com/"), 
      ", source code available on ")
    ),
    a(icon("github"), href = "https://github.com/inSilecoInc/shinySpatialApp"),
    HTML("</h4>")
)

