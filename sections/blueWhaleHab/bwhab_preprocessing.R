source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))
source(here::here("config.R"))

loadResult <- load_rdata(c("CommonData", "ebsa_rr"), regionStr)


# -----------Blue Whale habitat-------------- 
blueWhaleHabPkgId <- "8fafd919-fcbe-43a3-a911-3d9461273441"
blueWhaleResId <- "3af8ad03-c0da-4cfa-940d-d757c0c24cb7"

blueWhaleCheckDate <- get_check_date("blueWhaleHab_rr")
openBwhab_rr <- get_opendata_rr(blueWhaleHabPkgId, blueWhaleResId, checkDate = blueWhaleCheckDate)
if(!is.null(openBwhab_rr)) {
  blueWhaleHab_rr <- openBwhab_rr
  blueWhaleTemp_sf <- blueWhaleHab_rr$data_sf
  blueWhaleTemp_sf <- setNames(blueWhaleTemp_sf, replace(names(blueWhaleTemp_sf), names(blueWhaleTemp_sf) == 'activité', 'activite'))
  blueWhaleTemp_sf$activity[blueWhaleTemp_sf$activity == "foraging/Feeding"] <- "Foraging/Feeding"
  blueWhaleTemp_sf$activity[blueWhaleTemp_sf$activity == "Migrant"] <- "Migration"
  blueWhaleTemp_sf$months[blueWhaleTemp_sf$months == "all year"] <- "All year"
  blueWhaleTemp_sf$months[blueWhaleTemp_sf$months == "December to February/March to May"] <- "Dec-Feb/Mar-May"
  blueWhaleTemp_sf$months[blueWhaleTemp_sf$months == "December to February/June to August"] <- "Dec-Feb/Jun-Aug"
  blueWhaleTemp_sf$months[blueWhaleTemp_sf$months == "March to May/June to August"] <- "Mar-May/Jun-Aug"
  blueWhaleTemp_sf$Activity <- paste(blueWhaleTemp_sf$activity,"-", blueWhaleTemp_sf$months)
  blueWhaleTemp_sf <- sf::st_crop(blueWhaleTemp_sf, region_sf)
  blueWhaleHab_rr$data_sf <- blueWhaleTemp_sf
  
  blueWhaleHab_rr$metadata$contact <- email_format("gddaiss-dmsaisb\\@dfo-mpo.gc.ca")
  blueWhaleHab_rr$metadata$qualityTier <- highQuality
  blueWhaleHab_rr$attribute <- "Activity"
  blueWhaleHab_rr$metadata$reference <- lang_list("<https://waves-vagues.dfo-mpo.gc.ca/Library/40687776.pdf>")
  
  save(blueWhaleHab_rr, file = file.path(localFileSavePath, "Open/blueWhaleHab_rr.RData"))
}

