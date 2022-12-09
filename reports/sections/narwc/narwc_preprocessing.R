source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "narwc_rr"), regionStr)

# ----------------------NARWC-------------------------
# North Atlantic Right Whale Consortium (narwc)
if (globalControlEnv$updateGeoms) {
    
  narwcExcel <- readxl::read_xlsx(path = file.path(fileLoadPath, "NaturalResources/Species/Cetaceans/NARWC/NARWC_02-23-2022.xlsx"))
  narwcspecies <-  read.csv(file.path(fileLoadPath, "NaturalResources/Species/Cetaceans/NARWC/NARWCSpeciesNames.csv"), stringsAsFactors = FALSE)
  narwcspecies <- narwcspecies %>% dplyr::rename("Scientific Name"= ScientificName)
  narwc <- merge(narwcExcel, narwcspecies, by='SPECNAME')
  narwc <- narwc %>% dplyr::filter(YEAR >= rrMinYear)
  narwc <- merge(narwc, cetLegend, by = 'Scientific Name')
  narwc <- dplyr::select(narwc, 'Scientific Name', YEAR, Legend, LATITUDE, LONGITUDE)
  narwc_sf <- sf::st_as_sf(narwc, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  narwc_sf <- sf::st_crop(narwc_sf, region_sf)
} else {
  narwc_sf <- narwc_rr$data_sf
}

narwc_rr <- list("title" = "North Atlantic Right Whale consortium",
                 "data_sf" = narwc_sf,
                 "attribute" = "Legend",
                 "metadata" = read_google_metadata("narwc_rr")
)

save(narwc_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Secure/narwc_rr.RData"))
