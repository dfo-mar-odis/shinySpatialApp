source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))

loadResult <- load_rdata(c("CommonData", "sardist_rr"), regionStr)


filter_and_union <- function(sciName, sfObj) {
  filter_sf <- dplyr::filter(sfObj, `Scientific Name`==sciName)
  out_sf <- filter_sf %>%
    dplyr::group_by(`Scientific Name`, `Common Name`, Species_Link) %>%
    dplyr::summarize(geometry = sf::st_union(geometry))
  return(out_sf)
}

# -----------SAR DIST--------------
# Creating the sardist rr object takes a LOT (>10gb) of ram which may cause errors
# it is useful to run in a clean rstudio session and if needed, to manually
# step through the functions.
sardistPkgId <- "e0fabad5-9379-4077-87b9-5705f28c490b"
sardistResId <- "50c03ed9-a99f-4dd4-8c4e-2ad9d81432ef"
sardistCheckDate <-  get_check_date("sardist_rr")

openSardist_rr <- get_opendata_rr(sardistPkgId, sardistResId,
                                  region_sf = region_sf,
                                  gdbLayer = "DFO_SARA_Dist_2022_FGP_EN",
                                  checkDate = sardistCheckDate)
if(!is.null(openSardist_rr)) {
  sardist_rr <- openSardist_rr
  sardist_rr$metadata$qualityTier <- highQuality
  sardist_rr$metadata$contact <- email_format("info\\@dfo-mpo.gc.ca")
  sardist_rr$data_sf <- dplyr::select(sardist_rr$data_sf,
                                      c("Common_Name_EN", "Scientific_Name",
                                        "Species_Link", "Shape"))
  names(sardist_rr$data_sf) <- c("Common Name", "Scientific Name",
                                 "Species_Link", "geometry")
  st_geometry(sardist_rr$data_sf) <- "geometry"
  specNames <- names(table(sardist_rr$data_sf$`Scientific Name`))
  outList <- lapply(specNames, filter_and_union, sfObj=sardist_rr$data_sf)
  # !!! exapands the outlist in the argument, check ?"!!!"
  sardist_rr$data_sf <- dplyr::bind_rows(!!!outList)

  save(sardist_rr, file = file.path(localFileSavePath, "Open/sardist_rr.RData"))
}


# ------------------GEODATABASE VERSION------------------------

library(arcgisbinding)
arc.check_product() # you must have access to an ESRI arc license. Yuck :(

# sanity check the bridge:
filename <- system.file("extdata", "ca_ozone_pts.shp",
                        package = "arcgisbinding")
d <- arc.open(filename)
cat('all fields:', names(d@fields), fill = TRUE) # print all fields


get_sde_sf <- function(sdeLayerPath, whereClause = "OBJECTID > 0", isGeo=TRUE, cropRegion=FALSE) {
  sdeDataset <- arc.open(sdeLayerPath)
  sdeSelected <- arc.select(object = sdeDataset, where_clause = whereClause)
  if (isGeo) {
    outData <- arc.data2sf(sdeSelected) %>%
      sf::st_transform(sardists_sf, crs=4326) %>% 
      sf::st_make_valid()
    if (cropRegion) {
      outData <- sf::st_crop(outData, cropRegion)
    }
  } else {
    outData <- sdeSelected
  }
  return(outData)
}

somePath <- "C:\\Users\\stoyelq\\Desktop\\Work\\Reproducible_reports\\sara_database\\WEB.DFO_Regions"
arc.open(somePath)


sde_file <- system.file("C:/Users/stoyelq/Desktop/Work/Reproducible_reports/sara_database/VIEWER_SARA.sde/WEB.DFO_Regions",
                        package="arcgisbinding")
dataSDE <- arc.open(sde_file)

regionsPath <- here::here("../../sara_database/VIEWER_SARA.sde/WEB.DFO_Regions")
arc.open(regionsPath)

regions_sf <- get_sde_sf(regionsPath)

plot(dplyr::filter(regions_sf, DFO_REGION=="Maritimes")$geom)


crithabPath <- here::here("../../sara_database/VIEWER_SARA.sde/WEB.DFO_SARA_CritHab_Py")
draftCrithab_sf <- get_sde_sf(crithabPath, whereClause="CHSTATUS_E != 'Final'")


sardistPath <- here::here("../../sara_database/VIEWER_SARA.sde/WEB.DFO_SARA_Dist_Py")
sardistDataset <- arc.open(sardistPath)

sardists <- arc.select(object = sardistDataset, where_clause = "OBJECTID > 0")
sardists_sf <- arc.data2sf(sardists)
sardists_sf <- sf::st_transform(sardists_sf, crs=4326) %>% sf::st_make_valid()
finalCrithabs_sf <- sf::st_crop(crithabs_sf, region_sf)


crithabs_sf <- sf::st_transform(crithabs_sf, crs=4326) %>% sf::st_make_valid()
finalCrithabs_sf <- sf::st_crop(crithabs_sf, region_sf)

specList <- here::here("../../sara_database/VIEWER_SARA.sde/WEB.DFO_SARA_NationalList")
specListDataset <- arc.open(specList)

specLists <- arc.select(object = specListDataset, where_clause = "OBJECTID > 0")
crithabsOut <- left_join(finalCrithabs_sf, specLists, by="SPECIES_ID")
draftCH_sf <- dplyr::select(crithabsOut, c("CHSTATUS_E", "WATERBODY", "COMMON_E", 
                                           "POP_E", "SCIENTIFIC", "LEAD_REG_E", 
                                           "PROFILE_E"))

setwd(here::here())
renv::activate()



