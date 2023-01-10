source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))

loadResult <- load_rdata(c("CommonData", "finWhale_rr", "seiWhale_rr", 
                           "harbourPorpoise_rr", "humpbackWhale_rr"), regionStr)

print("-----------SDM--------------")

sdmPkgId <- "c094782e-0d6f-4cc0-b5a3-58908493a433"
sdmResId <- "16df15fb-367c-46e3-8ab7-be25315b9fbd"

if (globalControlEnv$updateGeoms) {
  finWhale_rr <-get_opendata_rr(sdmPkgId, sdmResId, region_sf = region_sf,
                                tifFile = "Fin_Whale.tif")
}
finWhale_rr$metadata <- read_google_metadata("sdm_rr", isOpenData = TRUE)

if (globalControlEnv$updateGeoms) {
seiWhale_rr <-get_opendata_rr(sdmPkgId, sdmResId, region_sf = region_sf,
                              tifFile = "Sei_Whale.tif")
}
seiWhale_rr$metadata <- read_google_metadata("sdm_rr", isOpenData = TRUE)

if (globalControlEnv$updateGeoms) {
humpbackWhale_rr <-get_opendata_rr(sdmPkgId, sdmResId, region_sf = region_sf,
                              tifFile = "Humpback_Whale.tif")
}
humpbackWhale_rr$metadata <- read_google_metadata("sdm_rr", isOpenData = TRUE)

if (globalControlEnv$updateGeoms) {
harbourPorpoise_rr <-get_opendata_rr(sdmPkgId, sdmResId, region_sf = region_sf,
                              tifFile = "Harbour_Porpoise.tif")
}
harbourPorpoise_rr$metadata <- read_google_metadata("sdm_rr", isOpenData = TRUE)

save(finWhale_rr, seiWhale_rr, humpbackWhale_rr, harbourPorpoise_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/sdm_rr.RData"))
