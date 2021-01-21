map <- selectionMap()
server <- function(input, output) {
  
  
    
  
    spa_all <- reactiveValues()
    # spa_all$vec => loadded list of geom
    spa_all$vec <- spa_all$spa_ext <- spa_all$spa_int <- NULL
    
    sel <- reactive(input$data_src)
    edits <- callModule(editMod, leafmap = map, id = "map")
    
    # ADD LAYERS 
    ## add existing layer(s)
    observeEvent(input$data_src, {
        # map <- leafem::addFeatures()
        spa_all$spa_int <- spa[as.numeric(sel())]
        spa_all$vec <- c(spa_all$spa_int, spa_all$spa_ext)
        map <- selectionMap(map, spa_all$vec)
        edits <- callModule(editMod, leafmap = map, id = "map")
    })
    ## add external layer 
    observeEvent(input$spa_ext, {
      spa_all$spa_ext <- c(spa_all$spa_int, list(readSpatial(input$spa_ext$datapath)))
      spa_all$vec <- c(spa_all$spa_int, spa_all$spa_ext)
      map <- selectionMap(map, spa_all$spa_ext)
      edits <- callModule(editMod, leafmap = map, id = "map")
    })
  
    
    # CREATE LAYER 
    observeEvent(input$save, {
        
        geom <- edits()$finished
        if (!is.null(geom)) {
            sf::write_sf(geom, 
              paste0('output/spatial/', input$name_geom, '.geojson'), delete_layer = TRUE, delete_dsn = TRUE)
        }
        if (input$geom_buffer) {
          msgInfo(input$geom_buffer, "m buffer to be added.")
        }
    })
    
    
    # SPATIAL OPERATION 
    observeEvent(input$oper_doit, {
      # TO DO 
      print(spatialOperation(input$oper_slc))
      print(input$name_output)
    })
    

    
    # REPORT
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