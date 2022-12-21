source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))
source(here::here("config.R"))

loadResult <- load_rdata(c("CommonData", "wsdb_rr"), regionStr)

print("------------------WSDB-------------------- ")
if (globalControlEnv$updateGeoms) {
  # Whale Sightings Database (wsdb)
  
  # Accessing API needs 2 parts: 1. the authentication token, and 2. the url to query.
  # There are 3-4 useful links/endpoints to make this happen.
  # This url is the mapserver url where the data is stored:
  egisUrl <- "https://gisd.dfo-mpo.gc.ca/arcgis/rest/services/SpatialReproducibleReporting/Map/MapServer/"
  # this url specifies the exact layer to pull data from:
  egisLayer <- paste0(egisUrl, "0/")
  
  # The info url is not explicitly needed but helps identify the token URL 
  infoUrl <-"https://gisd.dfo-mpo.gc.ca/arcgis/rest/info"
  # This is the token UR, it only accepts POST requests.
  tokenUrl <- "https://gisd.dfo-mpo.gc.ca/portal/sharing/rest/generateToken"
  
  # Use helper function to get a token. (use ctrl+click to see how it works)
  token <- get_token(tokenUrl, egisUrl)
  
  # Use esri2sf to access the data from the mapserver. 
  # This requires passing in the token as a param/header:
  wsdb_sf <- esri2sf::esri2sf(egisLayer, token=token, progress = TRUE)
  
  wsdb_sf <- dplyr::select(wsdb_sf, COMMONNAME, SCIENTIFICNAME, YEAR, LATITUDE, LONGITUDE)
  wsdb_sf <- wsdb_sf %>% dplyr::filter(YEAR >= rrMinYear)
  wsdb_sf <- dplyr::rename(wsdb_sf,c("Scientific Name" = "SCIENTIFICNAME",
                               "CNAME"= COMMONNAME))
  wsdb_sf <- merge(wsdb_sf, cetLegend, by='Scientific Name')
  wsdb_sf <- dplyr::select(wsdb_sf, CNAME, 'Scientific Name', YEAR, Legend, LATITUDE, LONGITUDE)
  wsdb_sf <- sf::st_crop(wsdb_sf, region_sf)
  
  wsdb_rr$data_sf <- wsdb_sf
}

wsdb_rr$metadata <- read_google_metadata("wsdb_rr", isOpenData = FALSE)
wsdb_rr$metadata$urlInternal <- "https://gisd.dfo-mpo.gc.ca/portal/home/webmap/viewer.html?webmap=7b4dd9e932864700a2f44ed70cc75b40&extent=-82.7098,36.4078,-30.8983,53.5042"

save(wsdb_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Secure/wsdb_rr.RData"))

