# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "janvrinIsland_rr"), regionStr)

# ---------------------Javrin Island Eelgrass-----------------------------------

esriUrl <- "https://gisd.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Netforce/MapServer/"
janvrinIsland_sf <- esri2sf::esri2sf(paste0(esriUrl, "1"), progress = TRUE)
janvrinIsland_sf$`Eelgrass Observation` <- janvrinIsland_sf$Eelgrass_Observation

janvrinIsland_sf <- sf::st_transform(janvrinIsland_sf, crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_crop(region_sf)

janvrinIsland_rr <- list("title" = "Janvrin Island Eelgrass Presence/Absence",
                         "data_sf" = janvrinIsland_sf,
                         "datasetName" = "Janvrin Island eelgrass",
                    "attribute" = "Eelgrass Observation",
                    "metadata" = list("contact" = email_format("JeanPierre.Ardouin@drdc-rddc.gc.ca"), 
                                      "accessedOnStr" = list("en" ="June 30, 2022", "fr" = "30 Juin, 2022") ,
                                      "accessDate" = as.Date("2022-06-30"),
                                      "searchYears" = "2005",
                                      "securityLevel" = noneList,
                                      "qualityTier" = highQuality,
                                      "constraints" = internalUse
                    )
                    
)
save(janvrinIsland_rr, file = file.path(localFileSavePath, "Open/janvrinIsland_rr.RData"))

