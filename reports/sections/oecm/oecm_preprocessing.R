source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "oecm_rr"), regionStr)

# ----------------OECM----------------- 
oecmPkgId <- "44769543-7a23-4991-a53f-c2cf7c7a946f"
oecmCheckDate <- get_check_date("oecm_rr")

openOecm_rr <- get_opendata_rr(oecmPkgId, region_sf = region_sf,
                               checkDate = oecmCheckDate)

esriBase <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Other_Effective_Area_Based_Conservation_Measures/MapServer/"
esriUrl <- paste0(esriBase, "0")
regionBbox <- sf::st_bbox(sf::st_transform(region_sf, 3857))
data_sf <- esri2sf::esri2sf(esriUrl, bbox=regionBbox, progress = TRUE)
openOecm_rr$data_sf <- sf::st_make_valid(data_sf)


if(!is.null(openOecm_rr)) {
  oecm_rr <- openOecm_rr
  oecm_rr$metadata$qualityTier <- highQuality
  oecm_rr$metadata$pipelinePath <- paste0(githubRepo, "reports/sections/oecm/oecm_preprocessing.R")

  save(oecm_rr, file = file.path(localFileSavePath, "Open/oecm_rr.RData"))
}

