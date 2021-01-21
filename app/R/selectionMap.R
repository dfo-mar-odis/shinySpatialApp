#   
selectionMap <- function(map = NULL, feat = NULL) {
  if (is.null(map)) {
    out <- leaflet::leaflet(height = 2000) %>%  
       leaflet::addTiles() %>% 
       # leafem::removeMouseCoordinates() %>%
       # add popup table?
       leafem::addMouseCoordinates() %>%
       leaflet::setView(lat = 45.6, lng = -63.6, zoom = 7) %>%
       leaflet::addProviderTiles('Esri.OceanBasemap', group = 'Ocean Basemap') %>%
       leaflet::addProviderTiles("OpenTopoMap", group = "OpenTopoMap") %>%
       leaflet::addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
       addLayersControl(
         baseGroups = c('OpenStreetMap', 'Ocean Basemap', 'OpenTopoMap'),
         position = 'bottomleft')
  } else out <- map

  if (!"sf" %in% class(feat)) {
    # to be improved
    for (i in seq_along(feat)) {
        out <- leafem::addFeatures(out, feat[[i]], popup = TRUE)
    } 
  } else { 
    out <- leafem::addFeatures(out, feat)
  }
  out 
}