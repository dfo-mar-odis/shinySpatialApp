source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "ebsa_rr"), regionStr)

# ----------------EBSA----------------- 
ebsaPkgId <- "d2d6057f-d7c4-45d9-9fd9-0a58370577e0"
ebsaCheckDate <- get_check_date("ebsa_rr")

openEbsa_rr <- get_opendata_rr(ebsaPkgId, region_sf = region_sf,
                               checkDate = ebsaCheckDate)

esriBase <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Ecologically_and_Biologically_Significant_Areas/MapServer/"
esriUrl <- paste0(esriBase, "0")
regionBbox <- sf::st_bbox(sf::st_transform(region_sf, 3857))
data_sf <- esri2sf::esri2sf(esriUrl, bbox=regionBbox, progress = TRUE)
openEbsa_rr$data_sf <- sf::st_make_valid(data_sf)


if(!is.null(openEbsa_rr)) {
  ebsa_rr <- openEbsa_rr
  ebsa_rr$metadata$qualityTier <- highQuality
  ebsa_rr$metadata$contact <- email_format("carissa.philippe\\@dfo-mpo.gc.ca")
  ebsa_rr$metadata$pipelinePath <- paste0(githubRepo, "reports/sections/ebsa/ebsa_preprocessing.R")

  save(ebsa_rr, file = file.path(localFileSavePath, "Open/ebsa_rr.RData"))
}

