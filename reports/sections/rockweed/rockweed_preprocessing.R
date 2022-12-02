source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "rockweed_rr"), regionStr)


# ---------------------ROCKWEED------------------------------
# Rockweed
rwServer <- "https://gisd.dfo-mpo.gc.ca/arcgis/rest/services/FGP/MARboundary_Rockweed_Presence_validated/MapServer/0/"
rwBbox <- sf::st_bbox(sf::st_transform(region_sf, 3857))
rockweed_sf <- esri2sf::esri2sf(rwServer, bbox = rwBbox, progress = TRUE)

rockweed_sf <- sf::st_make_valid(rockweed_sf)
# set status column
rockweed_sf$status = ""
rockweed_sf$status[which(rockweed_sf$RWP==1)] = "Present"
rockweed_sf$status[which(rockweed_sf$RWP==2)] = "Likely Present"
rockweed_sf$status[which(rockweed_sf$RWP==5)] = "Unknown"
final_rockweed_sf <- dplyr::select(rockweed_sf, c("status"))

rockweed_rr <- list("title" = "Satellite-based Maps of Intertidal Vegetation and Rockweed presence polygons",
                    "data_sf" = final_rockweed_sf,
                    "attribute" = "status",
                    "metadata" = read_google_metadata("rockweed_rr")
)
save(rockweed_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/rockweed_rr.RData"))

