
source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))

fileSavePath <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data\\RData\\data\\MAR"
fileSavePath <- here::here("app/data/MAR")
fileLoadPath <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data"

loadResult <- load_rdata(c("CommonData", "isdb_rr", "marfis_rr"),  "MAR")

highQuality <- list("en" = "High", "fr" = "Élevée")
mediumQuality <- list("en" = "Medium", "fr" = "Moyenne")
lowQuality <- list("en" = "Low", "fr" = "Faible")
noneList <- list("en" = "None", "fr"= "Aucun")
protectedBList <- list("en" = "Protected B", "fr" = "Protégé B")
internalUse <- list("en" = "DFO INTERNAL USE ONLY", "fr" = "DFO INTERNAL USE ONLY")


# -------------------ISDB------------------------------

# Load the ISDB and MARFIS species tables
load(file.path(fileLoadPath, "mar.wrangling/MARFIS.SPECIES.RData"))
load(file.path(fileLoadPath, "mar.wrangling/ISDB.ISSPECIESCODES.RData"))
SPECIES <- dplyr::select(SPECIES, one_of(c("SPECIES_CODE", "SPECIES_NAME")))
ISSPECIESCODES <- dplyr::select(ISSPECIESCODES, 
                                one_of(c("SPECCD_ID", "COMMON", "SCIENTIFIC")))

ISSPECIESCODES <- mutate(ISSPECIESCODES, 
                         SCIENTIFIC = str_to_sentence(SCIENTIFIC),
                         COMMON=str_to_sentence(COMMON))
ISSPECIESCODES <- ISSPECIESCODES %>% rename("Common Name" = COMMON,
                                            "Scientific Name" = SCIENTIFIC)
MARFISSPECIESCODES <- SPECIES %>% rename("COMMONNAME" = SPECIES_NAME)


# ISDB and MARFIS data
load(file.path(fileLoadPath, "mar.wrangling/isdb.RData"))
isdb_sf <- na.omit(isdb_sf) # remove NA values (some in SPECCID).  this is a bit slow
isdb_sf <- sf::st_crop(isdb_sf, region_sf)

isdb_rr <- list("title" = "Industry Survey Database (ISDB)",
                "data_sf" = isdb_sf,
                "attribute" = "NONE",
                "metadata" = list("contact" = email_format("Claire.Mussells@dfo-mpo.gc.ca"), 
                                  "accessedOnStr" = list("en" ="2019", "fr" = "2019") ,
                                  "accessDate" = as.Date("2019-01-01"),
                                  "searchYears" = "2010-2020",
                                  "securityLevel" = protectedBList,
                                  "qualityTier" = mediumQuality,
                                  "constraints" = internalUse)
)
save(isdb_rr, ISSPECIESCODES, file = file.path(fileSavePath, "Protected/isdb_rr.RData"))


load(file.path(fileLoadPath, "mar.wrangling/marfis.RData"))
marfis_sf <- sf::st_crop(marfis_sf, region_sf)

marfis_rr <- list("title" = "Maritime Fishery Information System (MARFIS)",
                  "data_sf" = marfis_sf,
                  "attribute" = "NONE",
                  "metadata" = list("contact" = email_format("XMARComData@dfo-mpo.gc.ca"),
                                    "accessedOnStr" = list("en" = "2019", "fr" = "2019") ,
                                    "accessDate" = as.Date("2019-01-01"),
                                    "searchYears" = "2010-2020",
                                    "securityLevel" = protectedBList,
                                    "qualityTier" = mediumQuality,
                                    "constraints" = internalUse)
)
save(marfis_rr, MARFISSPECIESCODES, file = file.path(fileSavePath, "Protected/marfis_rr.RData"))


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

save(ilts_rr, file = file.path(fileSavePath, "Protected/ilts_rr.RData"))

