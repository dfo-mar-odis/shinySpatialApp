source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))

loadResult <- load_rdata(c("CommonData", "sardist_rr"), regionStr)

filter_and_union <- function(sciName, sfObj) {
  filter_sf <- dplyr::filter(sfObj, `Scientific Name`==sciName)
  out_sf <- filter_sf %>%
  dplyr::group_by(`Scientific Name`, `Common Name`, Species_Link, Population, SARA_Status) %>%
  dplyr::summarize(geometry = sf::st_union(geometry))
  return(out_sf)
}

# -----------SAR DIST --------------
if (globalControlEnv$updateGeoms) {
    
  sardistPkgId <- "e0fabad5-9379-4077-87b9-5705f28c490b"
  sardist_rr <- get_opendata_rr(sardistPkgId)
  
  national_list <- file.path(fileLoadPath, "/NaturalResources/Species/SpeciesAtRisk/SaraDatabase/national_list.csv")
  nationalList <- read.csv(national_list)
  nationalList <- select(nationalList, c(Common.Name, Population, COSEWIC.Status))
  
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
                                            "Species_Link", "Population_EN", "SARA_Status",
                                            "geoms"))
  names(sardist_sf) <- c("Common Name", "Scientific Name",
                         "Species_Link", "Population", "SARA_Status", "geometry")
  st_geometry(sardist_sf) <- "geometry"
  
  # find species names to join on:
  specNames <- names(table(sardist_sf$`Scientific Name`))
  outList <- lapply(specNames, filter_and_union, sfObj=sardist_sf)
  # !!! exapands the outlist in the argument, check ?"!!!"
  sardist_sf <- bind_rows(!!!outList)
  
  dplyr::left_join(sardist_sf, nationalList, by=c("Common Name"="Common.Name", "Population"="Population"))$SARA.Status
  
  # save the data
  sardist_rr$data_sf <- sardist_sf
}

sardist_rr$metadata <- read_google_metadata("sardist_rr")

save(sardist_rr, file = file.path(localFileSavePath, "Open/sardist_rr.RData"))

