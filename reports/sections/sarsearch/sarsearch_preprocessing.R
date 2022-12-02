# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "sarsearch_rr"), regionStr)

print("---------------------SAR Search-----------------------------------")
if (globalControlEnv$updateGeoms) {
  # POINTS  
  url <- "http://dmapps/api/sar-search/points/?f=json"
  res <- httr::GET(url)
  raise <- httr::content(res, as="text")
  # parse JSON
  sarsearch_df <- jsonlite::fromJSON(raise)
  
  sarsearch_df$geometry <- lapply(sarsearch_df$geometry, function(geom) {
    outGeom <- NULL
    if (!is.null(geom)) {
      geom <- matrix(geom[, 2:1], ncol=2)
      outGeom <- sf::st_multipoint(geom)
    }
    return(outGeom)
  })
  
  sarsearch_sf <- sf::st_as_sf(sarsearch_df)
  sarsearch_sf <- sf::st_cast(sarsearch_sf, "POINT")
  
  # POLYGONS
  url <- "http://dmapps/api/sar-search/polygons/?f=json"
  res <- httr::GET(url)
  raise <- httr::content(res, as="text")
  # parse JSON
  poly_df <- jsonlite::fromJSON(raise)
  
  poly_df$geometry <- mapply(function(geomType, geom) {
    if (!is.null(geom)) {
      # swap the geom columns so lat-long is long-lat for sf
      geom <- matrix(geom[, 2:1], ncol=2)
      if (geomType == "line") {
        outGeom <- sf::st_linestring(geom)  
      } else {
        geomClosed <- rbind(geom, geom[1, ])
        outGeom <- sf::st_polygon(list(geomClosed))
      }
      
    } else {
      outGeom <- sf::st_polygon()
    }
    return(outGeom)
  }, poly_df$record_type, poly_df$geometry)
  
  poly_sf <- sf::st_as_sf(poly_df)
  sarsearch_sf <- rbind(sarsearch_sf, poly_sf)
  
  sarsearch_sf <- sarsearch_sf[!sf::st_is_empty(sarsearch_sf),,]
  sarsearch_sf <- dplyr::select(sarsearch_sf, "name", "source", "year", "notes", "species_name", "record_type", "geometry")
  sf::st_crs(sarsearch_sf) <- 4326
} else {
  sarsearch_sf <- sarsearch_rr$data_sf
}

# rr object structure:
sarsearch_rr <- list("title" = "DMapps SAR Search Database",
                     "data_sf" = sarsearch_sf,
                     "attributee" = "species_name",
              "metadata" = read_google_metadata("sarsearch_rr")
)
save(sarsearch_rr, file = file.path(localFileSavePath, "Secure/sarsearch_rr.RData"))

