source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))


loadResult <- load_rdata(c("CommonData", "IMRPebs_rr"), regionStr)

print("----------------IMRPEBSA----------------- ")
if (globalControlEnv$updateGeoms) {
    
  IMRPebsaPkgId <- "d2d6057f-d7c4-45d9-9fd9-0a58370577e0"
  
  IMRPebs_rr <- get_opendata_rr(IMRPebsaPkgId)
  
  esriBase <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Ecologically_and_Biologically_Significant_Areas/MapServer/"
  esriUrl <- paste0(esriBase, "0")
  regionBbox <- sf::st_bbox(sf::st_transform(region_sf, 3857))
  data_sf <- esri2sf::esri2sf(esriUrl, bbox=regionBbox, progress = TRUE)
  IMRPebs_rr$data_sf <- sf::st_make_valid(data_sf)
  
  additionalInfo <- read.csv("R:/Science/BIODataSvc/IN/MSP/Data/NaturalResources/Habitat/EBSA/customReportEBSAData.csv")
additionalInfo <- additionalInfo %>% 
  dplyr::mutate(PDF.Link = paste0("[Link](",additionalInfo$PDF.Link,")")) %>% 
  dplyr::mutate(Feature = gsub("[\\•]", "\\\n", Feature))


}


IMRPebs_rr$metadata <- read_google_metadata("ebsa_rr", isOpenData = TRUE)
IMRPebs_rr$attribute <- "Name"
save(IMRPebs_rr,additionalInfo, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/IMRPebs_rr.RData"))


