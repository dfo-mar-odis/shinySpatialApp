# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "opprrpe_rr"), regionStr)

# ---------------------TEMPLATE-----------------------------------
# LOADING AND PROCESSING CODE HERE
pkgId <- "f32ce23d-4a16-4eaa-9648-2f02a98b91af"
opprrpe_rr <- get_opendata_rr(pkgId)

esriUrl <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/OPP_RRP_Extents_En/MapServer/"
sj_sf <- esri2sf::esri2sf(paste0(esriUrl, "0"))
hawks_sf <- esri2sf::esri2sf(paste0(esriUrl, "1"))
opprrpe_sf <- rbind(sj_sf, hawks_sf)
names(opprrpe_sf)[names(opprrpe_sf) == 'Response_Area'] <- 'Response Area'

opprrpe_rr$data_sf <- sf::st_transform(opprrpe_sf, crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_crop(region_sf)


opprrpe_rr$attribute <- "Response Area"
opprrpe_rr$metadata$qualityTier <- highQuality
opprrpe_rr$metadata$pipelinePath <- paste0(githubRepo, "reports/sections/opprrpe/opprrpe_preprocessing.R")

save(opprrpe_rr, file = file.path(localFileSavePath, "Open/opprrpe_rr.RData"))

