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



################ EGIS Version ########################

# This url is the mapserver url where the data is stored:
egisUrl <- "https://gisd.dfo-mpo.gc.ca/arcgis/rest/services/SpatialReproducibleReporting/Industry_Survey_Database__ISDB_/MapServer/"
# 32 == 2010, 43 == 2021
layerList <- c(32:43)
# this url specifies the exact layer to pull data from:
egisLayers <- paste0(egisUrl, layerList)

# The info url is not explicitly needed but helps identify the token URL 
infoUrl <-"https://gisd.dfo-mpo.gc.ca/arcgis/rest/info"
# This is the token UR, it only accepts POST requests.
tokenUrl <- "https://gisd.dfo-mpo.gc.ca/portal/sharing/rest/generateToken"

# Use helper function to get a token. (use ctrl+click to see how it works)
token <- get_token(tokenUrl, egisUrl)

# Use esri2sf to access the data from the mapserver. 
# This requires passing in the token as a param/header:
regionBbox <- sf::st_bbox(sf::st_transform(region_sf, 3857))
isdb_sf_list <- lapply(egisLayers, esri2sf::esri2sf, bbox=regionBbox, token=token, progress = TRUE)

isdb_sf <- do.call(rbind, isdb_sf_list)

nafoGeoms_sf <- unique(dplyr::select(isdb_sf, "NAFO"))
isdbSpeceis_df <- unique(sf::st_drop_geometry(dplyr::select(isdb_sf, "NAFO", 
                                                            "COMMON", "SCIENTIFIC")))


isdb_rr <- list("title" = "Industry Survey Database (ISDB)",
                "data_sf" = nafoGeoms_sf,
                "attribute" = "NONE",
                "metadata" = list("contact" = email_format("Claire.Mussells@dfo-mpo.gc.ca"), 
                                  "accessedOnStr" = list("en" ="2022", "fr" = "2022") ,
                                  "accessDate" = as.Date("2022-10-07"),
                                  "searchYears" = "2010-2021",
                                  "securityLevel" = protectedBList,
                                  "qualityTier" = mediumQuality,
                                  "constraints" = lapply(internalUse, paste0, ". CONFIDENTIAL – AN ASSESSMENT IS REQUIRED BEFORE PUBLIC RELEASE.")
                )
)
save(isdb_rr, isdbSpeceis_df, file = file.path(localFileSavePath, "Protected/isdb_rr.RData"))



################ MARFIS EGIS Version ########################

# This url is the mapserver url where the data is stored:
egisUrl <- "https://gisd.dfo-mpo.gc.ca/arcgis/rest/services/SpatialReproducibleReporting/The_Maritime_Fishery_Information_System__MARFIS_/MapServer/"
# 8 == 2010, 19 == 2021
layerList <- c(8:19)
# this url specifies the exact layer to pull data from:
egisLayers <- paste0(egisUrl, layerList)

# The info url is not explicitly needed but helps identify the token URL 
infoUrl <-"https://gisd.dfo-mpo.gc.ca/arcgis/rest/info"
# This is the token UR, it only accepts POST requests.
tokenUrl <- "https://gisd.dfo-mpo.gc.ca/portal/sharing/rest/generateToken"

# Use helper function to get a token. (use ctrl+click to see how it works)
token <- get_token(tokenUrl, egisUrl)

# Use esri2sf to access the data from the mapserver. 
# This requires passing in the token as a param/header:
regionBbox <- sf::st_bbox(sf::st_transform(region_sf, 3857))
marfis_sf_list <- lapply(egisLayers, esri2sf::esri2sf, bbox=regionBbox, token=token, progress = TRUE)

marfis_sf <- do.call(rbind, marfis_sf_list)

nafoGeoms_sf <- unique(dplyr::select(marfis_sf, "NAFO"))


# add scientific names:
marfisSpeceis_df <- unique(sf::st_drop_geometry(dplyr::select(marfis_sf, "NAFO", 
                                                            "SPECIES_CODE", "SCIENTIFIC")))


isdb_rr <- list("title" = "Industry Survey Database (ISDB)",
                "data_sf" = nafoGeoms_sf,
                "attribute" = "NONE",
                "metadata" = list("contact" = email_format("Claire.Mussells@dfo-mpo.gc.ca"), 
                                  "accessedOnStr" = list("en" ="2022", "fr" = "2022") ,
                                  "accessDate" = as.Date("2022-10-07"),
                                  "searchYears" = "2010-2021",
                                  "securityLevel" = protectedBList,
                                  "qualityTier" = mediumQuality,
                                  "constraints" = lapply(internalUse, paste0, ". CONFIDENTIAL – AN ASSESSMENT IS REQUIRED BEFORE PUBLIC RELEASE.")
                )
)
save(isdb_rr, isdbSpeceis_df, file = file.path(localFileSavePath, "Protected/isdb_rr.RData"))










