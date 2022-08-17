source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))
source(here::here("config.R"))

loadResult <- load_rdata(c("CommonData", "wsdb_rr"), regionStr)

#------------------WSDB--------------------
# Whale Sightings Database (wsdb)
egisUrl <- "https://gisd.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Whale_Sightings_Database/MapServer/"
egisLayer <- paste0(egisUrl, "0/")

infoUrl <-"https://gisd.dfo-mpo.gc.ca/arcgis/rest/info"
tokenUrl <- "https://gisd.dfo-mpo.gc.ca/portal/sharing/rest/generateToken"

token <- get_token(tokenUrl, egisUrl)

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
