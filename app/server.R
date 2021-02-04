server <- function(input, output, session) {
  
      
  # INITIATE MAP
  map <- selectionMap()
  edits <- callModule(editMod, leafmap = map, id = "map")  
    
    
  # VALID AND STORE USER INFO
  valid_details <- reactive({
    # 2Bimproved
    output$valid_details <- renderText("")
    if (check_name(input$u_name)) {
      if (check_email(input$u_email)) {
        if (length(input$u_consent) == 3) {
          output$valid_details <- info_valid("All good!")
        } else {
          output$valid_details <- info_valid("Please abide to terms and conditions.", 
          FALSE)
        }
      } else {
        output$valid_details <- info_valid("Please enter a valid email.",
         FALSE)
      }
    } else {
      output$valid_details <- info_valid("Please enter a valid name.", 
        FALSE)
      }
  })
    
  observeEvent(input$get_u_details, { valid_details() })
  
  
  
  # CREATE GEOMS
  geoms <- reactiveValues(select = NULL, final = NULL)
  
  # GEOMS CREATED 
  nb_geom <- reactive({
    n <- ifelse(is.null(geoms$select), 0, nrow(geoms$select))
    output$nb_geoms <- info_valid(glue("Number of geoms saved so far: {n}"), n)
  })  
  observeEvent(geoms$select, { nb_geom() }, ignoreNULL = FALSE)
    
  ## From bbox
  observeEvent(input$save_bbox, {
    geom <- valid_bbox(input$bbox_xmin, input$bbox_xmax, input$bbox_ymin, input$bbox_ymax, glue("bbox_{input$save_bbox}"), input$bbox_crs)
    if (!is.null(geom)) {
      geoms$select <- append_geom(geoms$select, geom, input$bbox_buffer)
    }
  })
  
  ## From point
  observeEvent(input$save_pt, {
    geom <- valid_points(input$pt_x, input$pt_y, glue("point_{input$save_pt}"),
      input$pt_crs)
    if (!is.null(geom)) {
      geoms$select <- append_geom(geoms$select, geom, input$pt_buffer)
    }
  })
    
  ## From map 
  observeEvent(input$save_from_map, {
      
    output$from_map_not <- output$created_from_map <- renderText("")
    geom <- edits()$finished
    if (!is.null(geom)) {
      geoms$select <- append_geom(
        geoms$select, 
        valid_from_map(geom, glue("from_map_{input$save_from_map}")), 
        input$from_map_buffer
      )
    } else {
      output$from_map_not <- info_valid("Please use the map on the right.", FALSE)
    }
  })
    
  ## From import 
  shinyjs::hide(id = "import_buffer")
  shinyjs::hide(id = "save_import")
  observeEvent(input$import_shapefile, {
    shinyjs::show(id = "import_buffer")
    shinyjs::show(id = "save_import")
  })
  observeEvent(input$save_import, {
    geom <- valid_import(
      input$import_shapefile$datapath, 
      glue("imported_{input$save_import}")
    )
    if (!is.null(geom)) {
      geoms$select <- append_geom(geoms$select, geom, input$import_buffer)
    }
  })
    
    
    
  # VALID GEOM
  observeEvent(geoms$select, {
    
    if (is.null(geoms$select)) {
      x <- character(0)
    } else {
      x <- geoms$select$name
    }
    updateCheckboxGroupInput(session, "check_input_areas",
      label = "Select geoms",
      choiceNames = x,
      choiceValues = seq_along(x),
      selected = seq_along(x)
    )
  }, ignoreNULL = FALSE)
    

  observeEvent(input$check_input_areas, {
    n <- length(input$check_input_areas)
    output$nb_geoms_selected <- info_valid(glue("Number of geoms selected: {n}"), n)
    
    if (n) {
      shinyjs::show(id = "add_geoms_to_map")
      shinyjs::show(id = "valid_geoms")
    } else {
      shinyjs::hide(id = "add_geoms_to_map")
      shinyjs::hide(id = "valid_geoms")
    }
    
  }, ignoreNULL = FALSE)
  
  observeEvent(input$clear_map, {
    map <- selectionMap()
    callModule(editMod, leafmap = map, id = "map")
  })
      
  observeEvent(input$add_geoms_to_map, {
    map <- selectionMap(geoms$select[input$check_input_areas,])
    callModule(editMod, leafmap = map, id = "map")
  }, ignoreNULL = FALSE)
    
  observeEvent(input$valid_geoms, {
    n <- length(input$check_input_areas)
    output$nb_geoms_selected <- info_valid(glue("Number of geoms selected: {n}"), n, chk = TRUE)
    geoms$final <- geoms$select
  })
  
  
  
  
  # GENERATE REPORT
  values <- reactiveValues()
  values$generate <- 0
  shinyjs::hide(id = "dl_outputs")
  
  ## generate report
  output$report_html <- renderUI(includeHTML("www/empty_report.html"))
  observeEvent(input$generate_rmd, {
      
    if (length(input$u_consent) != 3) {
      output$render_success <- info_valid("Please abide to terms and conditions.", FALSE)  
    } else {
      chk <- renderReport(
          input = reactiveValuesToList(input), 
          geoms = geoms$final, 
          fl = input$report_name
        )
      if (chk$ok) {
        output$render_success <- info_valid(chk$msg, chk$ok)
        output$report_html <- renderUI({
        # NB Use a iframe so that the css of the report does not affect 
        # the css of the app
          tags$iframe(id = "iframe_report", src = chk$html, width = '100%',
            frameborder = 'no')
        })
        output$dl_outputs <- downloadHandler(
          filename = "output.zip",
          content = function(file) zip(file, "./output")
          )
        shinyjs::show(id = "dl_outputs")
      } else {
        output$render_success <- info_valid(chk$msg, FALSE)  
      }
    }

  })
    
}