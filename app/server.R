server <- function(input, output, session) {

  # INITIATE MAP
  map <- selectionMap(set_view = TRUE)
  edits <- callModule(editMod, leafmap = map, id = "map")

  # SWITCH MAIN TAB FROM MAP TO REPORT WHEN SIDE TAB IS REPORT
  observeEvent(input$active_panel, {
    if (input$active_panel == "Full report") {
      slc <- "Full report"
    } else if (input$active_panel == "Custom report") {
      slc <- "Custom report"
    } else slc <- "Map"
    updateTabsetPanel(session, "map_or_report", selected = slc)
  })

  # VALID AND STORE USER INFO
  u_details <- reactiveValues(
    name = NULL,
    email = NULL,
    notes = NULL,
    consent = FALSE
  )
  valid_details <- reactive({
    # Create a validator (see shinyvalidate)
    iv <- InputValidator$new()
    iv$add_rule("u_name", sv_required())
    iv$add_rule("u_email", sv_email())
    # iv$add_rule("u_notes", sv_required())
    iv$enable()

    output$valid_details <- renderText("")
    u_details$consent <- FALSE
    if (iv$is_valid()) {
      if (length(input$u_consent) == 3) {
        output$valid_details <- info_valid("All good!")
        u_details$name <- input$u_name
        u_details$email <- input$u_email
        u_details$notes <- input$u_notes
        u_details$consent <- TRUE
      } else {
        output$valid_details <- info_valid("Please abide by terms and conditions in the User tab.",
        FALSE)
      }
    } else {
      output$valid_details <- info_valid("Please fill out all required fields.",
        FALSE)
    } 
  })

  observeEvent(input$get_u_details, { valid_details() })



  # CREATE GEOMS
  geoms <- reactiveValues(select = NULL, final = NULL, locations = NULL)

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
    newnames <- paste0(
      dirname(input$import_shapefile$datapath), "/",
      input$import_shapefile$name
    )
    # for shapefiles, more than 1 file is uploaded
    # as we don't know the order of upload the following ensures `filename.shp`
    # will be read by sf (NB only 1 file will be read)
    detect_shp <- which(grepl("\\.shp$", newnames))
    if (!length(detect_shp)) detect_shp <- 1
    # file are renamed on upload so I renamed them cause sf need different file
    # component of a shapefile to have the same name
    file.rename(input$import_shapefile$datapath, newnames)
    geom <- valid_import(newnames[detect_shp[1]], glue("imported_{input$save_import}"))
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
  # Search location
  observeEvent(input$search_loc, {
    bb <- osmdata::getbb(input$location)
    if (!is.na(bb[1, 1])) {
      # https://github.com/r-spatial/sf/issues/572
      sf_loc <- st_as_sf(
          st_as_sfc(
            st_bbox(
              c(
                xmin = bb[1, 1],
                xmax = bb[1, 2],
                ymin = bb[2, 1],
                ymax = bb[2,2]
              )
            ),
            crs = 4326
          )
        )
      # review selection area so that default setup depends on selection
      map <- selectionMap(sf_loc, FALSE)
      callModule(editMod, leafmap = map, id = "map")
    }

  })


  observeEvent(input$check_input_areas, {
    n <- length(input$check_input_areas)
    output$nb_geoms_selected <- info_valid(glue("Number of geoms selected: {n}"), n)

    if (n) {
      shinyjs::show(id = "add_geoms_to_map")
      shinyjs::show(id = "valid_geoms")
      shinyjs::show(id = "select_view")
    } else {
      shinyjs::hide(id = "add_geoms_to_map")
      shinyjs::hide(id = "valid_geoms")
      shinyjs::hide(id = "select_view")
    }

  }, ignoreNULL = FALSE)

  # MAP VIEWS
  # Reset map when clicking on validate
  observeEvent(input$geometries, {
    if(input$geometries == "Validate") {
      map <- selectionMap(geoms$select[input$check_input_areas, ], FALSE) # Switch to TRUE to set it to default Halifax view
      callModule(editMod, leafmap = map, id = "map")
    }
  })

  # Clear all polygons
  observeEvent(input$clear_map, {
    leafletProxy("map-map") %>%
    clearShapes()
  })

  # Add selected polygons
  observeEvent(input$add_geoms_to_map, {
    if (!is.null(geoms)) {
      leafletProxy("map-map") %>%
      clearShapes() %>%
      leafem::addFeatures(geoms$select[input$check_input_areas, ])
    }
  })

  # Reset map in explore
  observeEvent(input$reset_view, {
    leafletProxy("map-map") %>%
    leaflet::setView(lat = 45.6, lng = -63.6, zoom = 7)
  })

  # Reset map in validate
  observeEvent(input$reset_view_valid, {
    leafletProxy("map-map") %>%
    leaflet::setView(lat = 45.6, lng = -63.6, zoom = 7)
  })

  # Set view on selected polygons
  observeEvent(input$select_view, {
    # print(st_bbox(geoms$select[input$check_input_areas, ]))
    bbv <- st_bbox(geoms$select[input$check_input_areas, ])
    leafletProxy("map-map") %>%
    leaflet::fitBounds(lng1 = bbv[[1]], lat1 = bbv[[2]], lng2 = bbv[[3]], lat2 = bbv[[4]])
  })


  observeEvent(input$valid_geoms, {
    n <- length(input$check_input_areas)
    output$nb_geoms_selected <- info_valid(glue("Number of geoms selected: {n}"), n, chk = TRUE)
    geoms$final <- geoms$select[input$check_input_areas, ]
  })

  # HIDE / SHOW FULL REPORT BASED ON REGION SELECTED IN USER TAB
  observeEvent(input$u_region, {
    if (input$u_region == "Gulf Region") {
      hideTab(inputId = "active_panel", target = "Full report")
      hideTab(inputId = "map_or_report", target = "Full report")
    } else {
      showTab(inputId = "active_panel", target = "Full report")
      showTab(inputId = "map_or_report", target = "Full report")
    }
  })

  # GENERATE REPORT
  preview <- reactiveValues(
    full_html = "www/empty_report.html",
    custom_html = "www/empty_report.html"
  )

  # FULL REPORT
  fullReportServer("tmp_full_report", geoms, preview, u_details)

  # FULL REPORT PREVIEW
  output$full_report_html <- renderUI({
      switch(
        preview$full_html,
        "www/empty_report.html" = includeHTML(preview$full_html),
        "www/wrong.html" = includeHTML(preview$full_html),
        {
          # not using include HTML here
          # remove www/ for inclusion in iframe
          html <- strsplit(preview$full_html, "www/")[[1]][2]
          # iframe used to isolate the css of the report from the css of the app
          tags$iframe(id = "iframe_report", src = html, width = '100%',
            frameborder = 'no')
        }
      )
    })


  # CUSTOM REPORT
  customReportServer("custom_report", geoms, preview, u_details)

  # CUSTOM REPORT PREVIEW
  output$custom_report_html <- renderUI({
    if (preview$custom_html == "www/empty_report.html") {
      includeHTML(preview$custom_html)
    } else {
      # remove www/ for inclusion in iframe
      html <- strsplit(preview$custom_html, "www/")[[1]][2]
      # NB Use a iframe so that the css of the report does not affect
      # the css of the app
      tags$iframe(id = "iframe_report", src = html, width = '100%',
        frameborder = 'no')
    }
  })

}
