# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "ssClam_rr"), regionStr)

# ---------------------Clam Presence-----------------------------------
pkgId <- "59121e8f-0acc-411a-99cb-54980df10ba6"
ssClam_rr <- get_opendata_rr(pkgId)

esriUrl <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/OPP_ARP_MAR_SpeciesPresence_Softshelled_Clams/MapServer/"
ssClam_sf <- esri2sf::esri2sf(paste0(esriUrl, "0"), progress = TRUE)
ssClam_sf <- dplyr::select(ssClam_sf, c("OVERALL_PRESENCE", "LIFE_STAGE", "RELATIVE_DISTRIBUTION",
                                    "RELATIVE_ABUNDANCE", "GEOGRAPHIC_AREA", "IMPORTANCE_RATIONALE", "JAN", "FEB",
                                    "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT",
                                    "NOV", "DEC"))
ssClam_sf$OVERALL_PRESENCE[ssClam_sf$OVERALL_PRESENCE == "see monthly presence"] <- "Verify with original record"



ssClam_rr$data_sf <- sf::st_transform(ssClam_sf, crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_crop(region_sf)

ssClam_rr$attribute <- "OVERALL_PRESENCE"
ssClam_rr$metadata <- read_google_metadata("Many")
ssClam_rr$datasetName <- "Likelihood of Presence of Soft Shelled Clams in Area Response Planning Pilot Areas"
save(ssClam_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/ssClam_rr.RData"))
