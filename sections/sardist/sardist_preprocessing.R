source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))

fileSavePath <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data\\RData\\data\\MAR"
fileSavePath <- here::here("app/data/MAR")
fileLoadPath <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data"

loadResult <- load_rdata(c("CommonData", "sardist_rr"), "MAR")


filter_and_union <- function(sciName, sfObj) {
  filter_sf <- dplyr::filter(sfObj, `Scientific Name`==sciName)
  out_sf <- filter_sf %>%
    group_by(`Scientific Name`, `Common Name`, Species_Link) %>% 
    summarize(geometry = st_union(geometry))
  return(out_sf)
}

# -----------SAR DIST--------------
# Creating the sardist rr object takes a LOT (>10gb) of ram which may cause errors
# it is useful to run in a clean rstudio session and if needed, to manually 
# step through the functions.
sardistPkgId <- "e0fabad5-9379-4077-87b9-5705f28c490b"
sardistResId <- "3b9b1fbf-59c5-4d2f-9a5c-289530ae410f"
sardistCheckDate <-  get_check_date("sardist_rr")

openSardist_rr <- get_opendata_rr(sardistPkgId, sardistResId, 
                                  region_sf = region_sf,
                                  gdbLayer = "DFO_SARA_Dist_2021_FGP_EN", 
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
  sardist_rr$data_sf <- bind_rows(!!!outList)
    
  save(sardist_rr, file = file.path(fileSavePath, "Open/sardist_rr.RData"))
}
