renv::install("ckanr")
install.packages("ckanr") #you need to install the packages every time since it is a fresh container

library(ckanr)

ckan_info(url = "https://open.canada.ca/data")
ckanr_setup(url="https://open.canada.ca/data")


ebsaPkg <- package_show("d2d6057f-d7c4-45d9-9fd9-0a58370577e0")
resList <- ebsaPkg$resources
shpRes <- resList[sapply(resList, function(item) {item$id == "ec990fd7-91b0-4dbb-a0f4-bb11070a84c1"})][[1]]

EBSA_sf <- download_extract_validate_sf(shpRes$url)
EBSA_sf$Report_URL <- str_replace(EBSA_sf$Report_URL, ".pdf", ".html")

download_extract_validate_sf <- function(zipUrl) {
  tempDir <- here::here("dataprocessing/temp")
  temp <- here::here("dataprocessing/temp/temp.zip")
  
  download.file(zipUrl, temp)
  utils::unzip(temp, exdir =tempDir)
  shpFile <- list.files(tempDir, recursive=TRUE, pattern="\\.shp$")
  
  out_sf <- st_read(shpFile)
  out_sf <- st_transform(out_sf, crs = 4326)
  out_sf <- st_make_valid(out_sf)
  
  # cleanup
  tempFiles <- list.files(tempDir, include.dirs = T, full.names = T, recursive = T)
  unlink(tempFiles, recursive = TRUE) 
  
  return(out_sf)
}
