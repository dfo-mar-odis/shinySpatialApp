# contains functions for downloading open data records
source(here::here("dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("app/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))
library(httr)

# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "lobsterEffort_rr"), regionStr)

# ---------------------TEMPLATE-----------------------------------

lobsterEffortPkgId <- "64f741d7-1129-49dd-9e5c-2b1de79024f0"

lobsterEffort_rr <- get_opendata_rr(lobsterEffortPkgId)

url <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Maritimes_Inshore_Lobster_Landings_Effort_2012_2014_EN/MapServer"
lobsterEffort_sf <- get_esri_rest(url, layer="1")
lobsterEffort_rr$data_sf <- sf::st_make_valid(lobsterEffort_sf)
# already in 4326 and cropped.

lobsterEffort_rr$attribute <- "weight_kg" 
lobsterEffort_rr$metadata <- read_google_metadata("lobsterEffort_rr")
save(lobsterEffort_rr, file = file.path(localFileSavePath, "Open/lobsterEffort_rr.RData"))

