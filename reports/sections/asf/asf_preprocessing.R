source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))
source(here::here("config.R"))

loadResult <- load_rdata(c("CommonData", "asf_rr"), regionStr)


if (globalControlEnv$updateGeoms) {
  
  #------------------WSDB--------------------
  # Whale Sightings Database (wsdb)
  # Accessing API needs 2 parts: 1. the authentication token, and 2. the url to query.
  # There are 3-4 useful links/endpoints to make this happen.
  # This url is the mapserver url where the data is stored:
  egisUrl <- "https://gisd.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Atlantic_Salmon_Rivers_of_Eastern_North_America/MapServer/"
  egisLayer <- paste0(egisUrl, "0/")
  
  # The info url is not explicitly needed but helps identify the token URL 
  infoUrl <-"https://gisd.dfo-mpo.gc.ca/arcgis/rest/info"
  # This is the token UR, it only accepts POST requests.
  tokenUrl <- "https://gisd.dfo-mpo.gc.ca/portal/sharing/rest/generateToken"
  
  # Use helper function to get a token. (use ctrl+click to see how it works)
  token <- get_token(tokenUrl, egisUrl)
  
  # Use esri2sf to access the data from the mapserver. 
  # This requires passing in the token as a param/header:
  regionBbox <- sf::st_bbox(sf::st_transform(region_sf, 3857))
  asf_sf <- esri2sf::esri2sf(egisLayer, bbox=regionBbox, token=token, progress = TRUE)
  asf_sf <- dplyr::select(asf_sf, NAME_LABEL, STATUS, geoms)
  names(asf_sf) <- c("River name", "Status", "geoms")
} else {
  asf_sf <- asf_rr$data_sf
}


asf_rr <- list("title" = "Atlantic salmon rivers of Eastern North America",
                "data_sf" = asf_sf,
                "attribute" = "Status",
                "metadata" = read_google_metadata("asf_rr")
)


save(asf_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Secure/asf_rr.RData"))
