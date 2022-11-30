source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "isdb_rr", "marfis_rr"), regionStr)


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
isdbSpecies_df <- unique(sf::st_drop_geometry(dplyr::select(isdb_sf, "NAFO", 
                                                            "COMMON", "SCIENTIFIC")))


isdb_rr <- list("title" = "Industry Survey Database (ISDB)",
                "data_sf" = nafoGeoms_sf,
                "attribute" = "NONE",
                "metadata" = read_google_metadata("isdb_rr", pipelinePath = paste0(githubRepo, "reports/sections/isdb-marfis/isdbMarfis_preprocessing.R"))
)
save(isdb_rr, isdbSpecies_df, file = file.path(localFileSavePath, "Protected/isdb_rr.RData"))



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
                                                            "SPECIES_NAME")))

marfis_rr <- list("title" = "Maritime Fishery Information System (MARFIS)",
                  "data_sf" = nafoGeoms_sf,
                  "attribute" = "NONE",
                  "metadata" = read_google_metadata("marfis_rr", pipelinePath = paste0(githubRepo, "reports/sections/isdb-marfis/isdbMarfis_preprocessing.R"))
)
save(marfis_rr, marfisSpeceis_df, file = file.path(localFileSavePath, "Protected/marfis_rr.RData"))





