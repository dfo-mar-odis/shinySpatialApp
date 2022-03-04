source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "rockweed_rr"), regionStr)


# ---------------------ROCKWEED------------------------------
# Rockweed
rockweed_sf <- st_read(file.path(fileLoadPath, "NaturalResources/Species/Rockweed/MAR_rockweed_presence_validated.shp"), stringsAsFactors = FALSE)
rockweed_sf <- st_transform(rockweed_sf, 4326) # Project to WGS84
rockweed_sf <- st_make_valid(rockweed_sf)
# set status column
rockweed_sf$status = ""
rockweed_sf$status[which(rockweed_sf$RWP==1)] = "Present"
rockweed_sf$status[which(rockweed_sf$RWP==2)] = "Likely Present"
rockweed_sf$status[which(rockweed_sf$RWP==5)] = "Unknown"
rockweed_sf$status[which(rockweed_sf$RWP==0)] = "Not Present"
rockweed_sf <- sf::st_crop(rockweed_sf, region_sf)


rockweed_rr <- list("title" = "Satellite-based Maps of Intertidal Vegetation and Rockweed presence polygons",
                    "data_sf" = rockweed_sf,
                    "attribute" = "status",
                    "metadata" = list("contact" = email_format("Gordana.Lazin@dfo-mpo.gc.ca"), 
                                      "accessedOnStr" = list("en" ="February 17 2021", 
                                                             "fr" = "17 février 2021") ,
                                      "accessDate" = as.Date("2021-02-17"),
                                      "searchYears" = "2020",
                                      "securityLevel" = noneList,
                                      "qualityTier" = mediumQuality,
                                      "constraints" = internalUse
                    )
)
save(rockweed_rr, file = file.path(localFileSavePath, "Open/rockweed_rr.RData"))

