source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "isdb_rr", "marfis_rr"), regionStr)


# -------------------ISDB------------------------------

# Load the ISDB and MARFIS species tables
load(file.path(fileLoadPath, "mar.wrangling/MARFIS.SPECIES.RData"))
load(file.path(fileLoadPath, "mar.wrangling/ISDB.ISSPECIESCODES.RData"))
SPECIES <- dplyr::select(SPECIES, one_of(c("SPECIES_CODE", "SPECIES_NAME")))
ISSPECIESCODES <- dplyr::select(ISSPECIESCODES, 
                                one_of(c("SPECCD_ID", "COMMON", "SCIENTIFIC")))

ISSPECIESCODES <- mutate(ISSPECIESCODES, 
                         SCIENTIFIC = stringr::str_to_sentence(SCIENTIFIC),
                         COMMON=stringr::str_to_sentence(COMMON))
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
save(isdb_rr, ISSPECIESCODES, file = file.path(localFileSavePath, "Protected/isdb_rr.RData"))


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
save(marfis_rr, MARFISSPECIESCODES, file = file.path(localFileSavePath, "Protected/marfis_rr.RData"))

