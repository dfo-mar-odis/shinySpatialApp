source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))


loadResult <- load_rdata(c("CommonData", "sardist_rr"), regionStr)

filter_and_union <- function(sciName, sfObj) {
  filter_sf <- dplyr::filter(sfObj, `Scientific Name`==sciName)
  out_sf <- filter_sf %>%
    group_by(`Scientific Name`, `Common Name`, Species_Link, `Population`) %>%
    summarize(geometry = sf::st_union(geometry))
  return(out_sf)
}

# -----------SAR DIST--------------
# get opendata metadata and set missing fields:
sardistPkgId <- "e0fabad5-9379-4077-87b9-5705f28c490b"
sardist_rr <- get_opendata_rr(sardistPkgId)
sardist_rr <- openSardist_rr

sardist_rr$metadata$qualityTier <- highQuality
sardist_rr$metadata$contact <- email_format("info\\@dfo-mpo.gc.ca")

# sara crs: 3857
# get crs with: esri2sf(url, where="OBJECTID<10", progress=TRUE)

regionBbox <- sf::st_bbox(sf::st_transform(region_sf, 3857))
url <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/DFO_SARA_Distribution/MapServer/0"
sardist_sf <- esri2sf::esri2sf(url, bbox=regionBbox, progress=TRUE)
# dump corrupt geoms, is_valid returns na for corrupt geoms:
sardist_sf <- sardist_sf[!is.na(sf::st_is_valid(sardist_sf)), ]
sardist_sf <- sf::st_make_valid(sardist_sf)

# trim and rename columns:
sardist_sf <- dplyr::select(sardist_sf, c("Common_Name_EN", "Scientific_Name",
                                          "Species_Link", "Population_EN", "geoms"))
names(sardist_sf) <- c("Common Name", "Scientific Name",
                       "Species_Link", "Population", "geometry")
st_geometry(sardist_sf) <- "geometry"

# find species names to join on:
specNames <- names(table(sardist_sf$`Scientific Name`))
outList <- lapply(specNames, filter_and_union, sfObj=sardist_sf)
# !!! exapands the outlist in the argument, check ?"!!!"
sardist_sf <- bind_rows(!!!outList)

# save the data
sardist_rr$data_sf <- sardist_sf
save(sardist_rr, file = file.path(localFileSavePath, "Open/sardist_rr.RData"))



# ------------------GEODATABASE VERSION------------------------
sf::sf_use_s2(FALSE) # because sf 1.0 is "broken" does not support treating spheres as flat


library(arcgisbinding)
arc.check_product() # you must have access to an ESRI arc license. Yuck :(


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
# esri mucks up your working dir too, joy!
setwd(here::here())



