source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))
source(here::here("config.R"))

loadResult <- load_rdata(c("CommonData", "wsdb_rr"), regionStr)

#------------------WSDB--------------------
# Whale Sightings Database (wsdb)

# Accessing API needs 2 parts: 1. the authentication token, and 2. the url to query.
# There are 3-4 useful links/endpoints to make this happen.
# This url is the mapserver url where the data is stored:
egisUrl <- "https://gisd.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Whale_Sightings_Database/MapServer/"
# this url specifies the exact layer to pull data from:
egisLayer <- paste0(egisUrl, "0/")

# The info url is not explicitly needed but helps identify the token URL 
infoUrl <-"https://gisd.dfo-mpo.gc.ca/arcgis/rest/info"
# This is the token UR, it only accepts POST requests.
tokenUrl <- "https://gisd.dfo-mpo.gc.ca/portal/sharing/rest/generateToken"

# Use helper function to get a token. (use ctrl+click to see how it works)
token <- get_token(tokenUrl, egisUrl)

# Use esri2sf to access the data from the mapserver. 
# This requires passing in the token as a param/header:
wsdb_sf <- esri2sf::esri2sf(egisLayer, token=token, progress = TRUE)

wsdb_sf <- dplyr::select(wsdb_sf, COMMONNAME, SCIENTIFICNAME, YEAR, LATITUDE, LONGITUDE)
wsdb_sf <- wsdb_sf %>% dplyr::filter(YEAR >= rrMinYear)
wsdb_sf <- dplyr::rename(wsdb_sf,c("Scientific Name" = "SCIENTIFICNAME",
                             "CNAME"= COMMONNAME))
wsdb_sf <- merge(wsdb_sf, cetLegend, by='Scientific Name')
wsdb_sf <- dplyr::select(wsdb_sf, CNAME, 'Scientific Name', YEAR, Legend, LATITUDE, LONGITUDE)
wsdb_sf <- sf::st_crop(wsdb_sf, region_sf)

wsdb_rr <- list("title" = "Whale Sightings Database",
                "data_sf" = wsdb_sf,
                "attribute" = "Legend",
                "metadata" = list("contact" = "<XMARWhaleSightings@dfo-mpo.gc.ca>", 
                                  "url" = lang_list("<http://www.inter.dfo-mpo.gc.ca/Maritimes/SABS/popec/sara/Database>"),
                                  "accessedOnStr" = list("en" ="October 27, 2020 by Shelley Lang", "fr" = "27 octobre 2020 par Shelley Lang  ") ,
                                  "accessDate" = as.Date("2020-10-27"),
                                  "searchYears" = paste(rrMinYear, "-2020", sep=""),
                                  "securityLevel" = noneList,
                                  "qualityTier" = lowQuality,
                                  "constraints" = internalUse
                )
)
save(wsdb_rr, file = file.path(localFileSavePath, "Secure/wsdb_rr.RData"))
