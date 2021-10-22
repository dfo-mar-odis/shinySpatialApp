
source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))


fileSavePath <- here::here("app/data/MAR")
fileLoadPath <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data"

load(here::here("app/data/CommonData.RData"))
loadResult <- load_rdata(c("isdb_rr"),  "MAR")

region_sf <- st_read(here::here("app/studyAreaTest/geoms_slc_MarBioRegion.geojson"))

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
SPECIES <- dplyr::select(SPECIES, 
                         one_of(c("SPECIES_CODE", "SPECIES_NAME")))
ISSPECIESCODES <- dplyr::select(ISSPECIESCODES, 
                                one_of(c("SPECCD_ID", "COMMON", "SCIENTIFIC")))

ISSPECIESCODES <- ISSPECIESCODES %>% transmute(ISSPECIESCODES, SCIENTIFIC=str_to_sentence(SCIENTIFIC))
ISSPECIESCODES <- ISSPECIESCODES %>% transmute(ISSPECIESCODES, COMMON=str_to_sentence(COMMON))
ISSPECIESCODES <- ISSPECIESCODES %>% rename("Common Name" = COMMON,
                                            "Scientific Name" = SCIENTIFIC)
MARFISSPECIESCODES <- SPECIES %>% rename("COMMONNAME" = SPECIES_NAME)


# ISDB and MARFIS data
load(file.path(fileLoadPath, "mar.wrangling/isdb.RData"))
isdb_sf <- na.omit(isdb_sf) # remove NA values (some in SPECCID).  this is a bit slow
isdb_sf <- sf::st_crop(isdb_sf, region_sf)

isdb_rr <- list("title" = "Industry Survey Database (ISDB)",
                       "contact" = email_format("Claire.Mussells@dfo-mpo.gc.ca"), 
                       "accessedOnStr" = list("en" ="2019", "fr" = "2019") ,
                       "accessDate" = as.Date("2019-01-01"),
                       "data_sf" = isdb_sf,
                       "attribute" = "NONE",
                       "securityLevel" = protectedBList,
                       "qualityTier" = mediumQuality,
                       "constraints" = internalUse
)
save(isdb_rr, ISSPECIESCODES, file = file.path(fileSavePath, "Protected/isdb_rr.RData"))


load(file.path(fileLoadPath, "mar.wrangling/marfis.RData"))
marfis_sf <- sf::st_crop(marfis_sf, region_sf)

marfis_rr <- list("title" = "Maritime Fishery Information System (MARFIS)",
                "contact" = email_format("XMARComData@dfo-mpo.gc.ca"), 
                "accessedOnStr" = list("en" ="2019", "fr" = "2019") ,
                "accessDate" = as.Date("2019-01-01"),
                "data_sf" = marfis_sf,
                "attribute" = "NONE",
                "securityLevel" = protectedBList,
                "qualityTier" = mediumQuality,
                "constraints" = internalUse
)
save(marfis_rr, MARFISSPECIESCODES, file = file.path(fileSavePath, "Protected/marfis_rr.RData"))



