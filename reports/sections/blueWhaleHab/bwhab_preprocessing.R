source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))
source(here::here("config.R"))

loadResult <- load_rdata(c("CommonData", "blueWhaleHab_rr"), regionStr)


# -------------------BWHAB-------------------
blueWhaleHabPkgId <- "8fafd919-fcbe-43a3-a911-3d9461273441"
blueWhaleHab_rr <- get_opendata_rr(blueWhaleHabPkgId)

url <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Biological_Sensitivity_Mapping_Oil_Spill_Planning_Response_Quebec_Region_en/MapServer/"
blueWhaleHab_sf <- get_esri_rest(url, layer="16")
blueWhaleHab_sf$Activity <- paste(blueWhaleHab_sf$DESCRIPTIO,"-", blueWhaleHab_sf$SEASONALIT)
blueWhaleHab_sf <- dplyr::select(blueWhaleHab_sf, c("Activity"))
blueWhaleHab_sf <- sf::st_crop(blueWhaleHab_sf, region_sf)
blueWhaleHab_rr$data_sf <- blueWhaleHab_sf

blueWhaleHab_rr$metadata$contact <- email_format("gddaiss-dmsaisb\\@dfo-mpo.gc.ca")
blueWhaleHab_rr$metadata$qualityTier <- highQuality
blueWhaleHab_rr$attribute <- "Activity"
blueWhaleHab_rr$metadata$reference <- lang_list("<https://waves-vagues.dfo-mpo.gc.ca/Library/40687776.pdf>")


save(blueWhaleHab_rr, file = file.path(localFileSavePath, "Open/blueWhaleHab_rr.RData"))




