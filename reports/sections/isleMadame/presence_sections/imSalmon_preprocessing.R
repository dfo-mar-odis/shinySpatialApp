# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "imSalmon_rr"), regionStr)

# --------------------- Presence-----------------------------------
pkgId <- "436cdf90-9d6b-4784-938b-feec48844a67"
imSalmon_rr <- get_opendata_rr(pkgId)

esriUrl <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/OPP_ARP_MAR_SpeciesPresence_AtlanticSalmon/MapServer/"
imSalmon_sf <- esri2sf::esri2sf(paste0(esriUrl, "0"), progress = TRUE)
imSalmon_sf <- dplyr::select(imSalmon_sf, c("OVERALL_PRESENCE", "LIFE_STAGE", "RELATIVE_DISTRIBUTION",
                                    "RELATIVE_ABUNDANCE", "GEOGRAPHIC_AREA", "IMPORTANCE_RATIONALE", "JAN", "FEB",
                                    "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", 
                                    "NOV", "DEC"))
imSalmon_sf$OVERALL_PRESENCE[imSalmon_sf$OVERALL_PRESENCE == "see monthly presence"] <- "Verify with original record"



imSalmon_rr$data_sf <- sf::st_transform(imSalmon_sf, crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_crop(region_sf)


imSalmon_rr$attribute <- "None"
imSalmon_rr$metadata$qualityTier <- mediumQuality
imSalmon_rr$datasetName <- "Atlantic Salmon Presence within the Bay of Fundy and Port Hawkesbury Response Plan areas"
save(imSalmon_rr, file = file.path(localFileSavePath, "Open/imSalmon_rr.RData"))

