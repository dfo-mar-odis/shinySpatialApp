source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))
source(here::here("config.R"))

loadResult <- load_rdata(c("CommonData", "pasBay_rr"), regionStr)

print("----------------------Passamaquoddy Bay Biodiversity Trawls---------------------")
if (globalControlEnv$updateGeoms) {
    
  pasBayPkgId <- "2dfa19db-a8cf-4460-97b9-710c2b856276"
  
  
  pasBay_rr <- get_opendata_rr(pasBayPkgId)
  
  catchDataResId <- "d128c2a5-8fa0-416d-a43d-af8d7131721b"
  catchData <- download_extract_res_files(catchDataResId)
  #catchData <- read.csv(file.path(fileLoadPath, "/NaturalResources/Species/PassamaquoddyBayBiodiversityTrawl/new_catch_data.csv"))
  catchData <- dplyr::select(catchData, c("set_ensemble", "scientificName_nomScientifique", "commonName_nomCommun"))
  
  
  setDataResId <- "d7b2316d-91ea-4896-bb9e-539d88008dac"
  #set data has header characters that are not supported under a normal read.csv call
  #therefore slightly altering this script to circumvent this
  res <- resource_show(setDataResId)
  zipUrl <- res$url
  setData <- read.csv(zipUrl, check.names = FALSE)
  rm(res,zipUrl)
  colnames(setData) = gsub("<e9>", "e", colnames(setData))
  setData <- setData %>% rename("longitude_finish_longitude_d.arrivee" = `longitude_finish_longitude d'arrivee`, "latitude_finish_latitude_d.arrivee" = "latitude_finish_latitude_d'arrivee")
  
  pasBay <- left_join(catchData, setData, by = "set_ensemble")
  pasBay <- dplyr::mutate(pasBay, 
                          wkt = paste("LINESTRING (", longitude_start_debut_de_longitude, " ", 
                                      latitude_start_debut_de_latitude, ", ", longitude_finish_longitude_d.arrivee, " ",
                                      latitude_finish_latitude_d.arrivee, ")", sep = ""))
  pasBay$geometry <- sf::st_as_sfc(pasBay$wkt, crs = 4326)
  pasBay <- dplyr::select(pasBay, c("scientificName_nomScientifique", "commonName_nomCommun", "time_start_heure_de_debut", "geometry"))
  pasBay$commonName_nomCommun<- stringr::str_to_title(pasBay$commonName_nomCommun)
  pasBay$scientificName_nomScientifique <- stringr::str_to_sentence(pasBay$scientificName_nomScientifique)
  names(pasBay) <-  c("Scientific Name", "Common Name", "Start Time", "geometry")
  
  pasBay_sf <- sf::st_as_sf(pasBay)
  pasBay_sf <- sf::st_crop(pasBay_sf, region_sf)
  pasBay_rr$data_sf <- pasBay_sf
}
pasBay_rr$metadata <- read_google_metadata("pasBay_rr", isOpenData = TRUE)


save(pasBay_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/pasBay_rr.RData"))

