source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))

fileSavePath <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data\\RData\\data\\MAR"
fileSavePath <- here::here("app/data/MAR")
fileLoadPath <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data"

loadResult <- load_rdata(c("CommonData", "ebsa_rr"), "MAR")

#----------------------Passamaquoddy Bay Biodiversity Trawls---------------------
pasBayPkgId <- "2dfa19db-a8cf-4460-97b9-710c2b856276"

pasBay_rr <- get_opendata_rr(pasBayPkgId, NULL, region_sf = region_sf)
pasBay_rr$metadata$contact <- email_format("Andrew.Cooper@dfo-mpo.gc.ca")
pasBay_rr$metadata$qualityTier <- highQuality
pasBay_rr$metadata$searchYears <- "2009-2017"

catchDataResId <- "5f55acd2-fe48-4624-a357-fe7babe2604b"
catchData <- download_extract_res_files(catchDataResId)
catchData <- read.csv(file.path(fileLoadPath, "/NaturalResources/Species/PassamaquoddyBayBiodiversityTrawl/new_catch_data.csv"))
catchData <- dplyr::select(catchData, c("setno", "scientific_name", "common_name"))

setDataResId <- "4184b6d7-b711-430c-81ed-504c05657c16"
setData <- download_extract_res_files(setDataResId)
# manual fix of typo on open data....
setData[(setData$setno == 2016.008),]$latitude_finish <- 45.09421

pasBay <- left_join(catchData, setData, by = "setno")
pasBay <- pasBay[!(is.na(pasBay$latitude_start)), ]
pasBay <- dplyr::mutate(pasBay, 
                        wkt = paste("LINESTRING (", longitude_start, " ", 
                                    latitude_start, ", ", longitude_finish, " ",
                                    latitude_finish, ")", sep = ""))
pasBay$geometry <- st_as_sfc(pasBay$wkt, crs = 4326)
pasBay <- dplyr::select(pasBay, c("scientific_name", "common_name", "time_start", "geometry"))
pasBay$common_name <- str_to_title(pasBay$common_name)
pasBay$scientific_name <- str_to_sentence(pasBay$scientific_name)
names(pasBay) <-  c("Scientific Name", "Common Name", "Start Time", "geometry")

pasBay_sf <- st_as_sf(pasBay)
pasBay_sf <- sf::st_crop(pasBay_sf, region_sf)
pasBay_rr$data_sf <- pasBay_sf
save(pasBay_rr, file = file.path(fileSavePath, "Open/pasBay_rr.RData"))

