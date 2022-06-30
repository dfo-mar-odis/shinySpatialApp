# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "harbourPorpoise_rr"), regionStr)

# ---------------------Bluefin Tuna Presence-----------------------------------
pkgId <- "58ea48ab-f052-48ab-9c18-4353e51b8bea"
harbourPorpoise_rr <- get_opendata_rr(pkgId)

esriUrl <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/OPP_ARP_MAR_SpeciesPresence_HarbourPorpoise/MapServer/"
harbourPorpoise_sf <- esri2sf::esri2sf(paste0(esriUrl, "0"), progress = TRUE)
harbourPorpoise_sf <- dplyr::select(harbourPorpoise_sf, c("OVERALL_PRESENCE", "LIFE_STAGE", "RELATIVE_DISTRIBUTION",
                                    "RELATIVE_ABUNDANCE", "GEOGRAPHIC_AREA", "IMPORTANCE_RATIONALE"))
harbourPorpoise_sf$OVERALL_PRESENCE[harbourPorpoise_sf$OVERALL_PRESENCE == "see monthly presence"] <- "Verify with original record"



harbourPorpoise_rr$data_sf <- sf::st_transform(harbourPorpoise_sf, crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_crop(region_sf)


harbourPorpoise_rr$attribute <- "None"
harbourPorpoise_rr$metadata$qualityTier <- highQuality
harbourPorpoise_rr$datasetName <- "Harbour Porpoise Presence"
save(harbourPorpoise_rr, file = file.path(localFileSavePath, "Open/harbourPorpoise_rr.RData"))

