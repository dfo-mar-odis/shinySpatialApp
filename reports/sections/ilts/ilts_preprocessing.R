source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "ilts_rr"), regionStr)

# --------------------ILTS-----------------------------
if (globalControlEnv$updateGeoms) {
    
  ilts <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/InshoreLobsterTrawlSurvey/ILTS.csv"), stringsAsFactors = FALSE)
  # set = start, haul = end
  iltsSpeciesCode <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/InshoreLobsterTrawlSurvey/SPECIESCODES.csv"), stringsAsFactors = FALSE)
  ilts <- dplyr::left_join(ilts, iltsSpeciesCode, by = "SPECIES_CODE")
  ilts <- dplyr::mutate(ilts, wkt = paste("LINESTRING (", SET_LONG, " ", SET_LAT, ", ", HAUL_LONG, " ", HAUL_LAT, ")", sep = ""))
  ilts$geometry <- sf::st_as_sfc(ilts$wkt, crs = 4326)
  ilts <- dplyr::select(ilts, c("HAUL_DATE", "COMMON.x", "SCIENTIFIC", "geometry"))
  ilts_sf <- sf::st_as_sf(ilts)
  
  ilts_sf$COMMON.x <- str_to_title(ilts_sf$COMMON.x)
  ilts_sf$SCIENTIFIC <- str_to_sentence(ilts_sf$SCIENTIFIC)
  ilts_sf$SCIENTIFIC[ilts_sf$COMMON.x == "Skate - Little Or Winter - Unspec."] <- "Leucoraja ocellata	(Uncertain)"
  ilts_sf$COMMON.x[ilts_sf$COMMON.x == "Skate - Little Or Winter - Unspec."] <- "Winter Skate (possible Little Skate)"
  names(ilts_sf) <- c("Date", "Common Name", "Scientific Name", "geometry")
  
  ilts_sf <- sf::st_crop(ilts_sf, region_sf)
} else {
  ilts_sf <- ilts_rr$data_sf
}

ilts_rr <- list("title" = "Inshore Lobster Trawl Survey (ILTS)",
                "data_sf" = ilts_sf,
                "attribute" = "NONE",
                "metadata" = read_google_metadata("ilts_rr")
)


save(ilts_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Protected/ilts_rr.RData"))
