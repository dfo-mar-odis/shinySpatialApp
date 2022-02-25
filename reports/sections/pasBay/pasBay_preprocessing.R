source(here::here("reports/R/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))
source(here::here("config.R"))

loadResult <- load_rdata(c("CommonData", "pasBay_rr"), regionStr)

#----------------------Passamaquoddy Bay Biodiversity Trawls---------------------
pasBayPkgId <- "2dfa19db-a8cf-4460-97b9-710c2b856276"

pasBay_rr <- get_opendata_rr(pasBayPkgId, NULL, region_sf = region_sf)
pasBay_rr$metadata$contact <- email_format("Andrew.Cooper@dfo-mpo.gc.ca")
pasBay_rr$metadata$qualityTier <- highQuality
pasBay_rr$metadata$searchYears <- "2009-2019"

catchDataResId <- "9ee042ea-ab9b-4af8-8cf5-de9c89a77a95"
catchData <- download_extract_res_files(catchDataResId)
catchData <- read.csv(file.path(fileLoadPath, "/NaturalResources/Species/PassamaquoddyBayBiodiversityTrawl/new_catch_data.csv"))
catchData <- dplyr::select(catchData, c("setno", "scientific_name", "common_name"))

setDataResId <- "b1379a08-7bbc-4ae7-b805-7e609e8d0f02"
setData <- download_extract_res_files(setDataResId)

pasBay <- left_join(catchData, setData, by = c("setno" = "set_ensemble"))
pasBay <- dplyr::mutate(pasBay, 
                        wkt = paste("LINESTRING (", longitude_start_début_de_longitude, " ", 
                                    latitude_start_début_de_latitude, ", ", longitude_finish_longitude.d.arrivée, " ",
                                    latitude_finish_latitude_d.arrivée, ")", sep = ""))
pasBay$geometry <- st_as_sfc(pasBay$wkt, crs = 4326)
pasBay <- dplyr::select(pasBay, c("scientific_name", "common_name", "time_start_heure_de_début", "geometry"))
pasBay$common_name <- stringr::str_to_title(pasBay$common_name)
pasBay$scientific_name <- stringr::str_to_sentence(pasBay$scientific_name)
names(pasBay) <-  c("Scientific Name", "Common Name", "Start Time", "geometry")

pasBay_sf <- st_as_sf(pasBay)
pasBay_sf <- sf::st_crop(pasBay_sf, region_sf)
pasBay_rr$data_sf <- pasBay_sf
save(pasBay_rr, file = file.path(localFileSavePath, "Open/pasBay_rr.RData"))

