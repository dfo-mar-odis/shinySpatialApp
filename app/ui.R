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
            
            # LAYER ADDITION 
            tabPanel(
              "Add layers", 
              icon = icon("database"),
              helpText("This tab alows you to add layers on map."),
              checkboxGroupInput("data_src", 
                                label = "Select the data you need", 
                                choiceNames = tb_ref$layer,
                                choiceValues = tb_ref$id),
                
              h4("Add a spatial file"),
              fileInput("spa_ext", "Choose a file",
              accept = c(".kml", ".tiff", ".shp", ".geojson")
              )
            ),
            
            # LAYER CREATION 
            tabPanel(
            "Create layers", 
            icon = icon("pencil"),
            helpText("This tab allows you to create and save layers."),
            h4("Create geom from map (geojson)"),
            textInput("name_geom", label = "Enter layer name (also used as file name)", value = "new_geom"),
            numericInput("geom_buffer", label = "Optional buffer (in meters)", value = "0", min = 0),
            actionButton('save', 'Create from Map', icon = icon("pencil")),
              
          ),
            
            # SPATIAL OPERATION(S) 
            tabPanel(
              "Spatial operation(s)", 
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
              textInput("report_name", label = "Report name", value = "report_01"),
              HTML("<h4>File selected: "),
              textOutput("file_report", inline = TRUE),
              HTML("</h4>"),
              hr(),
              actionButton("generate_rmd", "Generate report", icon("book")),
              htmlOutput("render_success"),
            )
          ),
            

            mainPanel(

)
          
            
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

