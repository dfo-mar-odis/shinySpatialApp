# contains functions for downloading open data records
source(here::here("dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("app/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "nitroLoad_rr"), regionStr)

# ----------------Nitro----------------- 
if (globalControlEnv$updateGeoms) {
    
  nitroPkgId <- "08746031-1970-4bf6-b6d4-3de2715c8634"
  nitroResId <- "cb3a3d96-bf2c-4403-a5c3-bed4ff54d1a5"
  
  esriBase <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Anthropogenic_Nitrogen_Eutrophication_Bay_of_Fundy_Scotian_Shelf_En/MapServer/"
  deltaUrl <- paste0(esriBase, "0")
  totalUrl <- paste0(esriBase, "1")
  
  
  nitroCheckDate <- get_check_date("nitroLoad_rr")
  
  totalNitroLoad_rr <- get_opendata_rr(nitroPkgId)
  total_sf <- esri2sf::esri2sf(totalUrl, progress=TRUE)
  totalNitroLoad_rr$data_sf <- sf::st_make_valid(total_sf)
  
  deltaNitroLoad_rr <- get_opendata_rr(nitroPkgId)
  delta_sf  <- esri2sf::esri2sf(deltaUrl, progress=TRUE)
  deltaNitroLoad_rr$data_sf <- sf::st_make_valid(delta_sf)
}
totalNitroLoad_rr$attribute <- "Total_Nitrogen_Load"
totalNitroLoad_rr$metadata <- read_google_metadata("nitroLoad_rr", isOpenData = TRUE)

deltaNitroLoad_rr$attribute <- "Mean_DeltaN"
deltaNitroLoad_rr$metadata <-  read_google_metadata("nitroLoad_rr", isOpenData = TRUE)


save(totalNitroLoad_rr, deltaNitroLoad_rr,
     file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/nitroLoad_rr.RData"))


