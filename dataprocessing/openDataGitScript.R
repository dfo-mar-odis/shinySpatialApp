install.packages("ckanr") #you need to install the packages every time since it is a fresh container
ckanr::ckanr_setup(url="https://open.canada.ca/data")

# ---------------GIT ACTION FUNCTIONS----------------
# get update date from package
date_from_pkg <- function(pkgId) {
  opendataPKG <- ckanr::package_show(pkgId)
  pkgTime <- NULL
  if ("date_modified" %in% names(opendataPKG)) {
    pkgTime <- strptime(opendataPKG$date_modified, "%Y-%m-%d %H:%M:%S")  
    pkgTime <- as.numeric(strftime(pkgTime, "%Y%m%d"))
  } else if ("metadata_modified" %in% names(opendataPKG)) {
    pkgTime <- strptime(opendataPKG$metadata_modified, "%Y-%m-%dT%H:%M:%S")  
    pkgTime <- as.numeric(strftime(pkgTime, "%Y%m%d"))
  }
  return(pkgTime)
}

check_for_open_data_updates <- function() {
  openDataData <- read.csv(here::here("dataprocessing/openDataData.csv"))
  
  openDataData$checkDate <- as.numeric(strftime(
    strptime(openDataData$Accessed.Date, "%Y-%m-%d"), "%Y%m%d"))
  openDataData$pkgDate <- lapply(openDataData$Package.Id, date_from_pkg)
  
  if (any(openDataData$pkgDate > openDataData$checkDate)) {
    updatePkgs <- subset(openDataData, pkgDate > checkDate)
    updatePkgs$msg <- paste("Update", updatePkgs$rr.Name, 
                            "object from opendata record:", updatePkgs$Title, 
                            "  \n\n")
    stop(updatePkgs$msg)
  }
}

# run the function
check_for_open_data_updates()
