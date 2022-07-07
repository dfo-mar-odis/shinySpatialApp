# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "harbourSeal_rr"), regionStr)

# ---------------------Bluefin Tuna Presence-----------------------------------
pkgId <- "5bbc1575-4267-44fa-ae35-ee08cc2af8fb"
harbourSeal_rr <- get_opendata_rr(pkgId)

esriUrl <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/OPP_ARP_MAR_SpeciesPresence_HarbourSeal/MapServer/"
harbourSeal_sf <- esri2sf::esri2sf(paste0(esriUrl, "0"), progress = TRUE)
harbourSeal_sf <- dplyr::select(harbourSeal_sf, c("OVERALL_PRESENCE", "LIFE_STAGE", "RELATIVE_DISTRIBUTION",
                                    "RELATIVE_ABUNDANCE", "GEOGRAPHIC_AREA", "IMPORTANCE_RATIONALE"))
harbourSeal_sf$OVERALL_PRESENCE[harbourSeal_sf$OVERALL_PRESENCE == "see monthly presence"] <- "Verify with original record"



harbourSeal_rr$data_sf <- sf::st_transform(harbourSeal_sf, crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_crop(region_sf)


harbourSeal_rr$attribute <- "None"
harbourSeal_rr$metadata$qualityTier <- mediumQuality
harbourSeal_rr$datasetName <- "Harbour Seal Presence"
save(harbourSeal_rr, file = file.path(localFileSavePath, "Open/harbourSeal_rr.RData"))

