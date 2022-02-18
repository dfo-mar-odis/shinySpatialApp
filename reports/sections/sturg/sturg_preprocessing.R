source(here::here("reports/R/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "sturg_rr"), regionStr)

# --------------------STURGEON---------------------
sturgDatafile <- file.path(here::here("../../../temp/Sturgeon_Layers/commondata/sturgeon/Sturgeon_AOI_polygons.shp"))
sturgData_sf <- st_read(sturgDatafile, crs = 4326)
sturgData_sf <- dplyr::select(sturgData_sf, c("Site", "geometry"))
sturgData_sf <- sf::st_crop(sturgData_sf, region_sf)
sturgData_sf$siteId <- rownames(sturgData_sf)

mudPidDatafile <- file.path(here::here("../../../temp/Sturgeon_Layers/commondata/mudpiddock/Mudpiddock_CriticalHabitat_Polygons.shp"))
mudPidData_sf <- st_read(mudPidDatafile, crs = 4326)
mudPidData_sf <- dplyr::select(mudPidData_sf, c("Site", "geometry"))
mudPidData_sf <- sf::st_crop(mudPidData_sf, region_sf)



sturg_rr <- list("title" = "Atlantic Sturgeon Foraging Areas in the Upper Bay of Fundy ",
                 "data_sf" = sturgData_sf,
                 "attribute" = "NONE",
                 "metadata" = list("contact" = paste("Brent Law (", email_format("Brent.Law@dfo-mpo.gc.ca"), ")", sep =), 
                                   "accessedOnStr" = list("en" ="September 21, 2021", "fr" = "21 septembre 2021") ,
                                   "accessDate" = as.Date("2021-09-21"),
                                   "securityLevel" = noneList,
                                   "qualityTier" = highQuality,
                                   "constraints" = internalUse
                 )
)
save(sturg_rr, file = file.path(localFileSavePath, "Secure/sturg_rr.RData"))
