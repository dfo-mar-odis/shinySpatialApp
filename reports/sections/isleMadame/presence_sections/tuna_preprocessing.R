# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "tuna_rr"), regionStr)

# ---------------------Bluefin Tuna Presence-----------------------------------
pkgId <- "0c3b25df-f831-43e8-a8ac-336e1467c4fe"
tuna_rr <- get_opendata_rr(pkgId)

esriUrl <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/OPP_ARP_MAR_SpeciesPresence_BluefinTuna/MapServer/"
tuna_sf <- esri2sf::esri2sf(paste0(esriUrl, "0"))
tuna_sf <- dplyr::select(tuna_sf, c("OVERALL_PRESENCE", "LIFE_STAGE", "RELATIVE_DISTRIBUTION",
                                    "RELATIVE_ABUNDANCE", "GEOGRAPHIC_AREA"))
tuna_sf$OVERALL_PRESENCE[tuna_sf$OVERALL_PRESENCE == "see monthly presence"] <- "Present June-November, likely present May, December"



tuna_rr$data_sf <- sf::st_transform(tuna_sf, crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_crop(region_sf)


tuna_rr$attribute <- "None"
tuna_rr$metadata$qualityTier <- mediumQuality
tuna_rr$datasetName <- "Bluefin Tuna Presence within the Bay of Fundy and Port Hawkesbury Response Plan areas"
save(tuna_rr, file = file.path(localFileSavePath, "Open/tuna_rr.RData"))
