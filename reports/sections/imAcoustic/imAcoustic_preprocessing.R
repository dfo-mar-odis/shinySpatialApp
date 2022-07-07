# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "imAcoustic_rr"), regionStr)

# ---------------------A Novel Video and Acoustic Survey of the Seaweeds of Isle Madame-----------------------------------
pkgId <- "ebdd8f91-9131-45f0-8aec-aba9f65e3fae"
imAcoustic_rr <- get_opendata_rr(pkgId)

esriUrl <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Novel_Video_Acoustic_Survey_Seaweeds_Isle_Madame_En/MapServer/"
imAcoustic_sf <- esri2sf::esri2sf(paste0(esriUrl, "4"), progress = TRUE)

imAcoustic_rr$data_sf <- sf::st_transform(imAcoustic_sf, crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_crop(region_sf)


imAcoustic_rr$attribute <- "None"
imAcoustic_rr$metadata$qualityTier <- highQuality
imAcoustic_rr$datasetName <- "A Novel Video and Acoustic Survey of the Seaweeds of Isle Madame"
save(imAcoustic_rr, file = file.path(localFileSavePath, "Open/imAcoustic_rr.RData"))

