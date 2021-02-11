# Interactive map
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
    
  if (!is.null(geoms)) out <- leafem::addFeatures(out, geoms)

  out
}

