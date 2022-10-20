source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "rv_rr"), regionStr)


#----------------------RV SURVEY---------------------
rvPkgId <- "8ddcaeea-b806-4958-a79f-ba9ab645f53b"

rv_rr <- get_opendata_rr(rvPkgId, NULL, region_sf = region_sf)
rv_rr$metadata$contact <- email_format("DFO.MAR-PED-Data-Request-Demande-de-donnes-DEP-MAR.MPO@dfo-mpo.gc.ca")
rv_rr$metadata$qualityTier <- highQuality

rvCsvList <- c("GSCAT.csv", "GSINF.csv", "GSSPECIES.csv")
# 4VSW
fourVSWResId <- "31439f60-2240-4c3e-bc57-34322ca214b0"
fourVSWDfs <- download_extract_res_files(fourVSWResId, rvCsvList)
fourVSW_sf <- RV_to_sf(fourVSWDfs[[1]], fourVSWDfs[[2]], fourVSWDfs[[3]], rrMinYear)
fourVSW_sf$surveyType <- "4VSW"
# Spring
springResId <- "6bb11439-9c65-4eef-bbff-81c32146dad7"
springDfs <- download_extract_res_files(springResId, rvCsvList)
spring_sf <- RV_to_sf(springDfs[[1]], springDfs[[2]], springDfs[[3]], rrMinYear)
spring_sf$surveyType <- "Spring"
# Summer
summerResId <- "f6b754c3-ebb3-4dc3-928f-6359c23975f2"
summerDfs <- download_extract_res_files(summerResId, rvCsvList)
summer_sf <- RV_to_sf(summerDfs[[1]], summerDfs[[2]], summerDfs[[3]], rrMinYear)
summer_sf$surveyType <- "Summer"
# Fall
fallResId <- "96912c03-5fcf-43e0-813e-1ef5390a213b"
fallDfs <- download_extract_res_files(fallResId, rvCsvList)
fall_sf <- RV_to_sf(fallDfs[[1]], fallDfs[[2]], fallDfs[[3]], rrMinYear)
summer_sf$surveyType <- "Fall"

rv_sf <- rbind(fourVSW_sf, spring_sf, summer_sf, fall_sf)
rv_sf <- sf::st_crop(rv_sf, region_sf)
rv_rr$data_sf <- rv_sf
rv_rr$metadata$searchYears <- paste(rrMinYear, "-2022", sep="")
save(rv_rr, file = file.path(localFileSavePath, "Open/rv_rr.RData"))


