source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "rv_rr"), regionStr)


#----------------------RV SURVEY---------------------
rvPkgId <- "8ddcaeea-b806-4958-a79f-ba9ab645f53b"

rv_rr <- get_opendata_rr(rvPkgId, NULL, region_sf = region_sf)
rv_rr$metadata$contact <- email_format("DFO.MAR-PED-Data-Request-Demande-de-donnes-DEP-MAR.MPO@dfo-mpo.gc.ca")
rv_rr$metadata$qualityTier <- highQuality

rvCsvList <- c("GSCAT.csv", "GSINF.csv", "GSSPECIES.csv")
minYear <- 2010
# 4VSW
fourVSWResId <- "ca308c48-2a87-4dcb-a4ba-90297b853635"
fourVSWDfs <- download_extract_res_files(fourVSWResId, rvCsvList)
fourVSW_sf <- RV_to_sf(fourVSWDfs[[1]], fourVSWDfs[[2]], fourVSWDfs[[3]], minYear)
fourVSW_sf$surveyType <- "4VSW"
# Spring
springResId <- "eef5180a-6a1b-4c85-aeb2-247afffb2077"
springDfs <- download_extract_res_files(springResId, rvCsvList)
spring_sf <- RV_to_sf(springDfs[[1]], springDfs[[2]], springDfs[[3]], minYear)
spring_sf$surveyType <- "Spring"
# Summer
summerResId <- "7f91a5ba-07dd-4494-b71d-89782cbb12bc"
summerDfs <- download_extract_res_files(summerResId, rvCsvList)
summer_sf <- RV_to_sf(summerDfs[[1]], summerDfs[[2]], summerDfs[[3]], minYear)
summer_sf$surveyType <- "Summer"
# Fall
fallResId <- "6abb48d0-384b-4c3e-8f13-56b74be919c4"
fallDfs <- download_extract_res_files(fallResId, rvCsvList)
fall_sf <- RV_to_sf(fallDfs[[1]], fallDfs[[2]], fallDfs[[3]], minYear)
summer_sf$surveyType <- "Fall"

rv_sf <- rbind(fourVSW_sf, spring_sf, summer_sf, fall_sf)
rv_sf <- sf::st_crop(rv_sf, region_sf)
rv_rr$data_sf <- rv_sf
rv_rr$metadata$searchYears <- "2010-2020"
save(rv_rr, file = file.path(localFileSavePath, "Open/rv_rr.RData"))


