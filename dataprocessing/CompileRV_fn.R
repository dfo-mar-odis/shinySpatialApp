# This function compiles the four RV survey sets into a single datafile 
# for use in the Reproducible Reporting project.
# It's called from the SaveDataSets_asRdata.R script.
#
# The RV survey data in the mar.wrangling/RVSurvey_FGP folder is 
# divided into four survey sets (Summer, Spring, Fall, 4VSW) and
# is stored as five related tables for each survey set
# GSCAT, GSDET, GSINF, GSSPECIES, GSMISSIONS)

# This function combines the representative tables for
# the four survey sets together
# (e.g. FALL_2020_GSCAT.csv, 4VSW_2020_GSCAT.csv, SPRING_2020_GSCAT.csv
# and SUMMER_2020_GSCAT.csv) and then merges the resultant
# related tables into a single dataframe which is converted to a 
# sf object
#
# 
#
# Written by Philip Greyson for Reproducible Reporting project, March 2021
#
########## - Compile RV data - #########################-

SurveyPrefix <- c("4VSW", "FALL", "SPRING", "SUMMER")
File <- c("_2020_GSCAT.csv", "_2020_GSINF.csv", "_2020_GSSPECIES.csv")
minYear <- 2010

CompileRV_fn <- function(SurveyPrefix, File, minYear) {
  
  RVdataPath = "../Data/mar.wrangling/RVSurvey_FGP"
  
  
  # Create single GSCAT table, rename the SPEC field to CODE
  f = File[1]
  tablelist <- list()
  for(i in 1:length(SurveyPrefix)) {
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

  # Join all GSCAT records that match those RV survey points AND join species
  # names to those records
  GSINF_sf <- left_join(GSINF_sf, GSCAT, by = "MISSION_SET")
  GSINF_sf <- left_join(GSINF_sf, GSSPECIES, by = "CODE")
  
  outList <- list(GSINF_sf, GSSPECIES)
  return(outList)
}



RV_to_sf <- function(gscat, gsinf, gsspec, minYear){
  gscat <- gscat %>% tidyr::unite("MISSION_SET", MISSION:SETNO, remove = TRUE)
  gscat <- gscat %>% rename(CODE = SPEC)
  
  gsinf <- gsinf %>% tidyr::unite("MISSION_SET", MISSION:SETNO, remove = FALSE)
  gsinf$YEAR <- lubridate::year(gsinf$SDATE)
  gsinf <- gsinf %>% dplyr::filter(YEAR >= minYear)
  
  gsspec <- dplyr::distinct(gsspec)
  gsspec <- gsspec %>% transmute(gsspec, SPEC = stringr::str_to_sentence(SPEC))
  gsspec <- gsspec %>% transmute(gsspec, COMM = stringr::str_to_sentence(COMM))
  gsspec <- gsspec %>% rename("Common Name"= COMM, "Scientific Name" = SPEC)
  
  out_sf <- sf::st_as_sf(gsinf, coords = c("SLONG", "SLAT"), crs = 4326)
  out_sf <- left_join(out_sf, gscat, by = "MISSION_SET")
  out_sf <- left_join(out_sf, gsspec, by = "CODE")
  
  out_sf <- out_sf %>% select("YEAR", "CODE", "Scientific Name", "Common Name", "TOTNO", "ELAT", "ELONG")
}


