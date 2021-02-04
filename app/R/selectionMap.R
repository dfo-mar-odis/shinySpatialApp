selectionMap <- function(geoms = NULL) {
  
  out <- leaflet::leaflet(height = 2000) %>% 
    leaflet::addTiles() %>% 
    leafem::addMouseCoordinates() %>%
    leaflet::setView(lat = 45.6, lng = -63.6, zoom = 7) %>%
    leaflet::addProviderTiles('Esri.OceanBasemap', group = 'OceaBasemap') %>%
    leaflet::addProviderTiles("OpenTopoMap", group = "OpenTopoMap") %>%
    leaflet::addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
    addLayersControl(
      baseGroups = c('OpenStreetMap', 'Ocean Basemap', 'OpenTopoMap'),
      position = 'bottomleft')
    

  if (!is.null(geoms)) {
    out <- leafem::addFeatures(out, geoms)
  } 
    # if (class(feat) == "list") {
    #   # to be improved
    # 
    #   for (i in seq_along(feat)) {
    #     print(feat[[i]])
    #     print(class(feat[[i]]))
    #       stopifnot(length(feat) == length(f_names))
    #       out <- leafem::addFeatures(out, feat[[i]]$geom, group = f_names[i])
    #   } 
    # } else { 
    #   out <- leafem::addFeatures(out, feat, group = f_names)
    # }
    
    # out <- out %>% addLayersControl(
    #   baseGroups = c('OpenStreetMap', 'Ocean Basemap', 'OpenTopoMap'),
    #   overlayGroups = f_names,
    #   position = 'bottomleft') %>% 
    #   hideGroup(f_names)
  # }
  out

}

