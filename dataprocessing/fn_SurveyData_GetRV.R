########## - Select RV data and intersect with the study area #########################-

SelectRV_fn <- function(SurveyPrefix, File, minYear) {
  
  RVdataPath = "../Data/mar.wrangling/RVSurvey_FGP"
  
  
  # Create single GSCAT table, rename the SPEC field to CODE
  f = File[1]
  tablelist <- list()
  for(i in 1:length(SurveyPrefix)) {
    #RVdataPath
    #df <- read.csv(file.path(RVdata.dir, paste(SurveyPrefix[i], f, sep = "", collapse = NULL)))
    df <- read.csv(file.path(RVdataPath, paste(SurveyPrefix[i], f, sep = "", collapse = NULL)))
    df <- df %>% tidyr::unite("MISSION_SET", MISSION:SETNO, remove = TRUE)
    # Keep only the columns necessary
    # ("MISSION_SET" "SPEC", "TOTNO")
    df <- dplyr::select(df,1:2,4)
    #Change column name
    df <- df %>% 
      rename(
        CODE = SPEC)
    tablelist[[i]] <- df
  }
  
  # combine all four RV survey catch tables together
  GSCAT <- rbind(tablelist[[1]],tablelist[[2]],tablelist[[3]],tablelist[[4]])
  
  # Create single GSINF table
  f = File[2]
  tablelist <- list()
  for(i in 1:length(SurveyPrefix)) {
    df <- read.csv(file.path(RVdataPath, paste(SurveyPrefix[i], f, sep = "", collapse = NULL)))
    df <- df %>% tidyr::unite("MISSION_SET", MISSION:SETNO, remove = FALSE)
    # Keep only the columns necessary
    # ("MISSION_SET", "MISSION", "SETNO", "SDATE", "SLAT", "SLONG", "ELAT", "ELONG")
    df <- dplyr::select(df,1:4,7:10,12)
    # Add YEAR and SEASON to the table
    df$YEAR <- lubridate::year(df$SDATE)
    df$SEASON <- SurveyPrefix[i]
    tablelist[[i]] <- df
  }
  # combine all four RV survey Information tables together
  # Filter down by Minumum Year
  GSINF <- rbind(tablelist[[1]],tablelist[[2]],tablelist[[3]],tablelist[[4]])
  GSINF <- GSINF %>% dplyr::filter(YEAR >= minYear)
  
  # Create single GSSPECIES table
  f = File[3]
  tablelist <- list()
  for(i in 1:length(SurveyPrefix)) {
    df <- read.csv(file.path(RVdataPath, paste(SurveyPrefix[i], f, sep = "", collapse = NULL)))
    # Remove TSN field
    df <- dplyr::select(df,(1:3))
    tablelist[[i]] <- df
  }
  
  # combine all four RV SPECIES tables together
  GSSPECIES <- rbind(tablelist[[1]],tablelist[[2]],tablelist[[3]],tablelist[[4]])
  # remove duplicate records
  GSSPECIES <- dplyr::distinct(GSSPECIES)
  GSSPECIES <- GSSPECIES %>% transmute(GSSPECIES, SPEC = str_to_sentence(SPEC))
  GSSPECIES <- GSSPECIES %>% transmute(GSSPECIES, COMM = str_to_sentence(COMM))
  GSSPECIES <- GSSPECIES %>% rename("Common Name"= COMM,
                                    "Scientific Name" = SPEC)
  
  # Convert GSINF to sf object
  GSINF_sf = st_as_sf(GSINF, coords = c("SLONG", "SLAT"), crs = 4326) #WGS84
  
  # Select all RV survey points within the Exposure Zone (studyArea) using st_intersect
  #  RVintersect <- st_intersection(GSINF_sf,studyArea)
  
  # Join all GSCAT records that match those RV survey points AND join species
  # names to those records
  GSINF_sf <- left_join(GSINF_sf, GSCAT, by = "MISSION_SET")
  GSINF_sf <- left_join(GSINF_sf, GSSPECIES, by = "CODE")
  
  outList <- list(GSINF_sf, GSSPECIES)
  return(outList)
}

########## - Select MARFIS data and intersect with the study area #########################-

SelectMARFIS_fn <- function(studyArea, minYear) {
  
  # SurveyPath <-  "../Data/mar.wrangling"
  
  #############################################-
  # to use this .RData file and convert to a SF object
  # load data file and species file
  
  filelist <- c(file.path(SurveyPath,"marfis.RData"), file.path(SurveyPath,"MARFIS.SPECIES.RData"))
  lapply(filelist, load, envir=.GlobalEnv)
  
  # Reduce MARFIS species table down to only species code, common name
  SPECIES <- dplyr::select(SPECIES,SPECIES_CODE, SPECIES_NAME)
  
  # add YEAR column and filter for records from minYear onwards
  marfis1$YEAR <- lubridate::year(marfis1$DATE_FISHED)
  marfis1 <- marfis1 %>% dplyr::filter(YEAR >= minYear)
  
  # Convert to SF object
  marfis1 <- st_as_sf(marfis1, coords = c("LONGITUDE", "LATITUDE"))
  st_crs(marfis1) <-  4326
  
  # Select all MARFIS points within the Exposure Zone (PEZ) using st_intersect
  Catch <- st_intersection(marfis1,studyArea)
  # merge the data file with species names using common species codes
  Catch <- merge(Catch,SPECIES, by = 'SPECIES_CODE')
  
  return(Catch)
  
}

########## - Select ISDB data and intersect with the study area #########################-

SelectISDB_fn <- function(studyArea, minYear) {
  # SurveyPath = "../Data/mar.wrangling"
  
  filelist <- c(file.path(SurveyPath,"isdb.RData"), file.path(SurveyPath,"ISDB.ISSPECIESCODES.RData"))
  lapply(filelist, load, envir=.GlobalEnv)
  
  # Reduce MARFIS species table down to only species code, common name
  ISSPECIESCODES <- dplyr::select(ISSPECIESCODES,SPECCD_ID,COMMON, SCIENTIFIC)
  
  # add YEAR column and filter for records from minYear onwards
  
  isdb1$DATA_TIME1 <- lubridate::parse_date_time(isdb1$DATE_TIME1, orders = "ymd")
  
  isdb1$YEAR <- lubridate::year(isdb1$DATA_TIME1)
  isdb1 <- isdb1 %>% dplyr::filter(YEAR >= minYear)
  
  # Convert to SF object
  isdb1 <- st_as_sf(isdb1, coords = c("LONGITUDE", "LATITUDE"))
  st_crs(isdb1) <-  4326
  
  # Select all MARFIS points within the Exposure Zone (PEZ) using st_intersect
  Catch <- st_intersection(isdb1,studyArea)
  # merge the data file with species names using common species codes
  Catch <- merge(Catch,ISSPECIESCODES, by = 'SPECCD_ID')
  
  return(Catch)
  
}