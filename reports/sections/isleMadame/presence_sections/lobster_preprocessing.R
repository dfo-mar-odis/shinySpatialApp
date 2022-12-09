# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "imLobster_rr"), regionStr)

print("---------------------Lobster Presence-----------------------------------")
if (globalControlEnv$updateGeoms) {
    
  pkgId <- "47bf4555-ce3c-492f-a367-a6eab1862970"
  imLobster_rr <- get_opendata_rr(pkgId)
  
  esriUrl <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/OPP_ARP_MAR_SpeciesPresence_Lobster/MapServer/"
  imLobster_sf <- esri2sf::esri2sf(paste0(esriUrl, "0"), progress = TRUE)
  imLobster_sf <- dplyr::select(imLobster_sf, c("OVERALL_PRESENCE", "LIFE_STAGE", "RELATIVE_DISTRIBUTION",
                                      "RELATIVE_ABUNDANCE", "GEOGRAPHIC_AREA", "IMPORTANCE_RATIONALE", "JAN", "FEB",
                                      "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT",
                                      "NOV", "DEC"))
  imLobster_sf$OVERALL_PRESENCE[imLobster_sf$OVERALL_PRESENCE == "see monthly presence"] <- "Verify with original record"
  
  
  
  imLobster_rr$data_sf <- sf::st_transform(imLobster_sf, crs = 4326) %>%
    sf::st_make_valid() %>%
    sf::st_crop(region_sf)
}

imLobster_rr$attribute <- "None"
imLobster_rr$metadata <- read_google_metadata("Many")
imLobster_rr$datasetName <- "Likelihood of Presence of American Lobster in Area Response Planning Pilot Areas"
save(imLobster_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/imLobster_rr.RData"))
