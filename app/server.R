server <- function(input, output, session) {
  
  # Store all data needed for the report reactively
  data_in <- reactiveValues()
  # list of data and areas 
  data_in$data_src <- data_in$geoms <- list()
    
    
  # INITIATE MAP
  map <- selectionMap()
  consent <- spa_all <- reactiveValues()
  consent <- 0
  edits <- callModule(editMod, leafmap = map, id = "map")  
    
    
  # VALID AND STORE USER INFO
  valid_details <- reactive({
    output$invalid_details <- output$valid_details <- renderText("")
    data_in$valid_details <- FALSE
    if (check_name(input$u_name)) {
      data_in$user <- input$u_name
      if (check_email(input$u_email)) {
        data_in$email <- input$u_email
        consent <- length(input$u_consent)
        if (consent == 3) {
          output$valid_details <- info_valid("All good!")
          data_in$notes <- input$u_notes
          data_in$valid_details <- TRUE
        } else {
          output$invalid_details <- info_valid("Please abide to terms and conditions.", 
          FALSE)
        }
      } else {
        output$invalid_details <- info_valid("Please enter a valid email.", FALSE)
      }
    } else {
      output$invalid_details <- info_valid("Please enter a valid name.",FALSE)
      }
  })
    
  observeEvent(input$get_u_details, { valid_details() })
  
  
  
  # CREATE LAYER 
    
  ## From bbox (2Bdone)
    
  ## From map 
  observeEvent(input$save_from_map, {
      
    output$created_from_map_not <- output$created_from_map <- renderText("")
    geom <- edits()$finished
    if (!is.null(geom)) {
      nm <- glue('output/spatial/{input$name_geom}.geojson')
      if (!file.exists(nm)) {
        sf::write_sf(geom, nm ,delete_layer = TRUE, delete_dsn = TRUE)
        output$created_from_map <- info_valid(glue('{nm}.geojson created'))
        data_in$geoms <- append(data_in$geoms, 
          list(list(geom = geom, name = basename(nm), method = "drawn"))
        )
      } else { 
        output$created_from_map_not <- info_valid("Cannot overwrite existing file", FALSE)
      }
    } else {
      output$created_from_map_not <- info_valid("Please use the map on the right.", FALSE)
    }

  })
    
  ## From external shapefile
  spa_ext <- reactiveValues()
  # ## add external layer 
  shinyjs::hide(id = "shp_to_map")
  shinyjs::hide(id = "save_shp")
  observeEvent(input$import_shapefile, {
    shinyjs::show(id = "shp_to_map")
    shinyjs::show(id = "save_shp")
  })
  observeEvent(input$shp_to_map, {
    geom_ext <- readSpatial(input$import_shapefile$datapath)
    map2 <- map %>% leafem::addFeatures(geom_ext)
    edits <- callModule(editMod, leafmap = map2, id = "map")
    # spa_ext <- readSpatial(input$spa_ext$datapath)
  })
  observeEvent(input$save_shp, {
    geom_ext <- readSpatial(input$import_shapefile$datapath)
    data_in$geoms <- append(data_in$geoms, 
        list(list(geom = geom_ext, name = input$import_shapefile$name, 
          method = "external")))
  })
    
    
    
    # SPATIAL OPERATION (deprecated)
    
    # Selection of area (previously created)
    observe({
    
      if (!length(data_in$geoms)) {
        x <- character(0)
      } else {
        x <- unlist(
          lapply(data_in$geoms, 
            function(y) glue('{y$name} ({y$method})'))
          )
      }
      updateCheckboxGroupInput(session, "check_input_areas",
        label = 'Select',
        choices = x,
        selected = x
      )
    })


  # GENERATE REPORT
  values <- reactiveValues()
  values$generate <- 0

    # rmd_int <- reactive({
    #   output$rmd_path <- renderText(input$int_rmd)
    #   output$file_report <- renderText(input$int_rmd)
    #   values$rmd_path <- input$int_rmd
    #   isolate({values$generate <- 0})
    # })
    # observeEvent(input$int_rmd, { rmd_int() })
    # 
    ## select a file
    # rmd_ext <- reactive({
    #   output$rmd_path <- renderText(input$ext_rmd$datapath)
    #   output$file_report <- renderText(input$ext_rmd$name)
    #   values$rmd_path <- input$ext_rmd$datapath
    # })
    # observeEvent(input$ext_rmd, { rmd_ext() })
  
  ## generate report
  output$report_html <- renderUI(includeHTML("www/empty_report.html"))
  observeEvent(input$generate_rmd, {
      
    if (length(input$u_consent) != 3) {
      output$render_success <- info_valid("Please first abide to terms an s conditions (first tab).", FALSE)  
    } else {
      chk <- renderReport(data_in, reactiveValuesToList(input), 
        fl = input$report_name)
      if (chk$ok) {
        output$render_success <- info_valid("All good")
        output$report_html <- renderUI({
        # Use a iframe so that the css of the report does not affect 
        # the css of the app
          tags$iframe(id = "iframe_report", src = chk$html, width = '100%',
            frameborder='no')
        })
      } else 
         output$render_success <- info_valid("Issue while rendering", FALSE)  
      }

    })
    
}