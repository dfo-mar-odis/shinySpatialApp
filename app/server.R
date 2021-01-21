server <- function(input, output, session) {
  
    # Store all data needed for the report reactively
    data_in <- reactiveValues()
    # list of data and areas 
    data_in$data_src <- data_in$geoms <- list()
    
    
    
    # INITIATE MAP
    map <- selectionMap()
    spa_all <- reactiveValues()
    edits <- callModule(editMod, leafmap = map, id = "map")  
    
    
    
    # VALID AND STORE USER INFO
    valid_details <- reactive({
      output$invalid_details <- output$valid_details <- renderText("")
      if (check_name(input$user_name)) {
        data_in$user <- input$user_name
        if (check_email(input$user_email)) {
          data_in$mail <- input$user_mail
          if (input$user_consent) {
            output$valid_details <- renderText("All good!")
            data_in$valid_details <- TRUE
          } else {
            output$invalid_details <- renderText("You must abide to the terms.")
            data_in$valid_details <- FALSE
          }
        } else {
          output$invalid_details <- renderText("Please enter a valid email.")
        }
      } else {
        output$invalid_details <- renderText("Please enter a valid name.")
      }
    })
    
    observeEvent(input$get_user_details, { valid_details() })
  
  
  

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
              output$created_from_map <- renderText(glue('{nm}.geojson created'))
              data_in$geoms <- append(data_in$geoms, 
                list(list(geom = geom, name = basename(nm), method = "drawn")))
            } else output$created_from_map_not <- renderText("Cannot overwrite existing file")
        } else {
          output$created_from_map_not <- renderText("Please use the map on the right.")
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
            list(list(geom = geom_ext, name = input$import_shapefile$name, method = "external")))
    })
    
    
    
    
    
    
    # SPATIAL OPERATION 
    
    ## Selection of existing data source
    
    
    ## Selection of area (previously created)
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


    ## Select operation 
    observeEvent(input$oper_doit, {
      # TO DO 

    })






    
    # GENERATE REPORT
    values <- reactiveValues()
    values$generate <- 0
    ## select files available
    rmd_int <- reactive({
      output$rmd_path <- renderText(input$int_rmd)
      output$file_report <- renderText(input$int_rmd)
      values$rmd_path <- input$int_rmd
      isolate({values$generate <- 0})
    })
    observeEvent(input$int_rmd, { rmd_int() })
    
    ## select a file
    rmd_ext <- reactive({
      output$rmd_path <- renderText(input$ext_rmd$datapath)
      output$file_report <- renderText(input$ext_rmd$name)
      values$rmd_path <- input$ext_rmd$datapath
    })
    observeEvent(input$ext_rmd, { rmd_ext() })
  
    ## generate report
    observeEvent(input$generate_rmd, {
      # intermediate var, so that Rmd not being rendered when values$rmd_path changes
      # but only when we click 
      path <- values$rmd_path
      output$render_success <- renderText(renderReport(path, fl = input$report_name))
    })
    
    
}