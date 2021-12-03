source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "ilts_rr"), regionStr)

# --------------------ILTS-----------------------------

ilts <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/InshoreLobsterTrawlSurvey/ILTS.csv"), stringsAsFactors = FALSE)
# set = start, haul = end
iltsSpeciesCode <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/InshoreLobsterTrawlSurvey/SPECIESCODES.csv"), stringsAsFactors = FALSE)
ilts <- dplyr::left_join(ilts, iltsSpeciesCode, by = "SPECIES_CODE")
ilts <- dplyr::mutate(ilts, wkt = paste("LINESTRING (", SET_LONG, " ", SET_LAT, ", ", HAUL_LONG, " ", HAUL_LAT, ")", sep = ""))
ilts$geometry <- st_as_sfc(ilts$wkt, crs = 4326)
ilts <- dplyr::select(ilts, c("HAUL_DATE", "COMMON.x", "SCIENTIFIC", "geometry"))
ilts_sf <- st_as_sf(ilts)

ilts_sf$COMMON.x <- str_to_title(ilts_sf$COMMON.x)
ilts_sf$SCIENTIFIC <- str_to_sentence(ilts_sf$SCIENTIFIC)
ilts_sf$SCIENTIFIC[ilts_sf$COMMON.x == "Skate - Little Or Winter - Unspec."] <- "Leucoraja ocellata	(Uncertain)"
ilts_sf$COMMON.x[ilts_sf$COMMON.x == "Skate - Little Or Winter - Unspec."] <- "Winter Skate (possible Little Skate)"
names(ilts_sf) <- c("Date", "Common Name", "Scientific Name", "geometry")

ilts_sf <- sf::st_crop(ilts_sf, region_sf)

ilts_rr <- list("title" = "Inshore Lobster Trawl Survey (ILTS)",
                "data_sf" = ilts_sf,
                "attribute" = "NONE",
                "metadata" = list("contact" = email_format("Cheryl.Denton@dfo-mpo.gc.ca"),
                                  "url" = list("en" = "<https://gcgeo.gc.ca/geonetwork/metadata/eng/190276ba-29ea-4b3b-be74-757d0eb896f2>",
                                               "fr" = "<https://gcgeo.gc.ca/geonetwork/metadata/fre/190276ba-29ea-4b3b-be74-757d0eb896f2>"),
                                  "accessedOnStr" = list("en" = "October 1, 2021 by Geraint Element", 
                                                         "fr" = "1 Octobre, 2021 par Geraint Element") ,
                                  "accessDate" = as.Date("2020-10-01"),
                                  "searchYears" = "2016-2021",
                                  "securityLevel" = protectedBList,
                                  "qualityTier" = highQuality,
                                  "constraints" = internalUse)
)

save(ilts_rr, file = file.path(localFileSavePath, "Protected/ilts_rr.RData"))
