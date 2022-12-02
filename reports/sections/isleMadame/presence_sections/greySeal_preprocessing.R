# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "greySeal_rr"), regionStr)

print("---------------------Bluefin Tuna Presence-----------------------------------")
pkgId <- "e73c90ff-0ab6-4257-8d6d-3dfc46fc0dc5"
greySeal_rr <- get_opendata_rr(pkgId)

esriUrl <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/OPP_ARP_MAR_SpeciesPresence_GreySeal/MapServer/"
greySeal_sf <- esri2sf::esri2sf(paste0(esriUrl, "0"), progress = TRUE)
greySeal_sf <- dplyr::select(greySeal_sf, c("OVERALL_PRESENCE", "LIFE_STAGE", "RELATIVE_DISTRIBUTION",
                                    "RELATIVE_ABUNDANCE", "GEOGRAPHIC_AREA", "IMPORTANCE_RATIONALE", "JAN", "FEB",
                                    "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT",
                                    "NOV", "DEC"))
greySeal_sf$OVERALL_PRESENCE[greySeal_sf$OVERALL_PRESENCE == "see monthly presence"] <- "Verify with original record"



greySeal_rr$data_sf <- sf::st_transform(greySeal_sf, crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_crop(region_sf)


greySeal_rr$attribute <- "None"
greySeal_rr$metadata <- read_google_metadata("Many")
greySeal_rr$datasetName <- "Likelihood of Presence of Grey Seal in Area Response Planning Pilot Areas"
save(greySeal_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/greySeal_rr.RData"))
