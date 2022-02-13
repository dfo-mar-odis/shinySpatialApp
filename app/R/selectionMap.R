# Interactive map created with `leaflet` and called in `mapedit::callModule`.

selectionMap <- function(geoms = NULL, set_view = TRUE) {
  
  out <- leaflet::leaflet(height = 2000) %>% 
    leaflet::addTiles() %>% 
    leafem::addMouseCoordinates() %>%
    leaflet::addProviderTiles('Esri.OceanBasemap', group = 'OceaBasemap') %>%
    leaflet::addProviderTiles("OpenTopoMap", group = "OpenTopoMap") %>%
    leaflet::addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
    addLayersControl(
      baseGroups = c('OpenStreetMap', 'Ocean Basemap', 'OpenTopoMap'),
      position = 'bottomleft')
    
  if (!is.null(geoms)) {
    out <- leafem::addFeatures(out, geoms) 
  } else {
    out <- leaflet::setView(out, lat = 45.6, lng = -63.6, zoom = 7)    
  }
  
  if (set_view) {
    out %>% leaflet::setView(lat = 45.6, lng = -63.6, zoom = 7)
  } else out 
}

