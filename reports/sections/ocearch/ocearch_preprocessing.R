source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "ocearch_rr"), regionStr)


print(" --------------------OCEARCH---------------------")
# LOAD OCEARCH DATA #############
if (globalControlEnv$updateGeoms) {
  
  ocearchDatafile <- file.path(fileLoadPath, "NaturalResources/Species/Sharks/OCEARCH/OCEARCH_08-27-2021.csv")
  lines <- readLines(ocearchDatafile)
  lines <- gsub('(^"|"$)', "", lines)
  ocearch <- read.csv(textConnection(lines), quote = '""')
  ocearch <- dplyr::select(ocearch, c("Date", "long", "lat", "ID"))
  ocearch_sf <- sf::st_as_sf(ocearch, coords = c("long", "lat"), crs = 4326)
  ocearch_sf <- sf::st_crop(ocearch_sf, region_sf)
} else {
  ocearch_sf <- ocearch_rrr$data_sf
}

ocearch_rr <- list("title" = "OCEARCH Shark Tracker",
                   "data_sf" = ocearch_sf,
                   "attribute" = "NONE",
                   "metadata" = read_google_metadata("ocearch_rr")
)
save(ocearch_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Secure/ocearch_rr.RData"))
