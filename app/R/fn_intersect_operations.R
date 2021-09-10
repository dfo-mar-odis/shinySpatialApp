# The functions in this file are used to clip various data sources (e.g. MARFIS, ISDB, etc.)
# with the studyArea selected and summarize the data for tables within the final document.
#
# 1. master_intersect() - clips POINT and POLYGON data to the extent of the studyArea, previously main_intersect()
# 2. create_table_RV() - creates summary tables of all species and listed species
# 3. create_table_MARFIS() - creates summary tables of all species and listed species
# 4. create_table_ISDB() - creates summary tables of all species and listed species
# 5. create_table_OBIS() - creates summary table of listed species
# 6. sfcoords_as_cols() - extracts latitude and longitude from geometry field
#                         and add them to new columns
#
# Written by Philip Greyson for Reproducible Reporting project, May/2021



##### - master_intersect function ##################################
# This function clips various data sources (e.g. MARFIS, ISDB, etc.)
# to the extent of the studyArea and the map bounding box.
# The map bounding box is created by the area_map() function
#
# Inputs:
# 1. data_sf: an input polygon vector file
# 2. region: a spatial file of the region
# 3. studyArea: polygon of the study area (sf object, defined by the user in the shiny app)
# 4. mapBbox: Coordinates of the map bounding box exported from area_map() function (bboxMap)
#
# Outputs: list containing 4 items
# 1. studyData: the full dataset from clipping data_sf by the studyArea
# 2. mapData: the full dataset from clipping data_sf by the Bounding box
# 3. regionData: the full dataset from clipping data_sf by the region
# 4. mapPoints: Unique collection of points to be plotted on a map.


master_intersect <- function(data_sf, mapDataList, getRegion=FALSE, ...) {

  # check that data_sf is an accepted format:
  if (!inherits(sf::st_geometry(data_sf), c("sfc_POINT", 
                                            "sfc_POLYGON", 
                                            "sfc_MULTIPOLYGON",
                                            "sfc_GEOMETRY")))
  {
    outList <- list(regionData = NULL,
                    studyData = NULL, 
                    mapData = NULL, 
                    mapPoints = NULL)
    return(outList)
  }
  
  # convert bbox to sf object representing the map
  mapArea <- sf::st_as_sfc(mapDataList$bboxMap)
  
  # Crop data
  if (getRegion) {
    regionData <- sf::st_crop(data_sf, mapDataList$region)  
    if (nrow(regionData) == 0) {regionData <- NULL}
  } else {
    regionData <- NULL
  }
  
  mapData <- sf::st_crop(data_sf, mapArea)
  studyData <- sf::st_crop(mapData, mapDataList$studyArea)
  
  # if there is no intersect with the box, set return to NULL
  if (nrow(mapData) == 0) {mapData <- NULL}
  
  if (nrow(studyData) > 0) {
    # if there is point data in the study area, drop uneeded columns and 
    # duplicate geometries
    # for RV the ELAT and ELONG fields are necessary as well
    if (inherits(sf::st_geometry(data_sf), "sfc_POINT")) {
      if ("ELAT" %in% colnames(mapData)) {
        mapPoints <- dplyr::select(mapData, ELAT, ELONG, geometry)
      } 
      else {
        mapPoints <- dplyr::select(mapData, geometry)
      }
      # remove redundant geometries and set lat/long columns:
      mapPoints <- unique(mapPoints)
      mapPoints <- sfcoords_as_cols(mapPoints)
    } # end of test for point geometry
    else {
      mapPoints <- NULL
    }
  } # end of test for zero samples
  else {
    studyData <- NULL
    mapPoints <- NULL
  }
  outList <- list(regionData = regionData,
                  studyData = studyData, 
                  mapData = mapData, 
                  mapPoints = mapPoints)
  return(outList)
}

  
##### - END master_intersect function ##################################


##### - raster_intersect function ##################################
# This function clips various raster data sources (e.g. SDM output, etc.)
# to the extent of the region, the studyArea, and the map bounding box.
# The map bounding box is created by the area_map() function
#
# Inputs:
# 1. datafile: an input raster file
# 2. region: a spatial file of the region
# 3. studyArea: polygon of the study area (sf object, defined by the user in the shiny app)
# 4. mapBbox: Coordinates of the map bounding box exported from area_map() function
#
# Outputs: list containing 3 items
# 1. studyRas: the full dataset from clipping the datafile by the studyArea
# 2. mapRas: the full dataset from clipping the datafile by the Bounding box
# 3. regionRas: the full dataset from clipping the datafile by the region
raster_intersect <- function(datafile, region, studyArea, mapBbox, ...) {

  # convert Bbox to sf object
  tmpBbox <- st_as_sfc(mapBbox)
  mapArea <- st_as_sf(tmpBbox)
  # Crop Data
  # with raster datasets it's necesssary to comnbine the
  # crop and mask functions
  regionData <- crop(datafile, region)
  regionData <- raster::mask(regionData, region)
  mapData <- crop(regionData, mapArea)
  mapData <- mask(mapData, mapArea)
  studyData <- crop(mapData, studyArea)
  studyData <- mask(studyData, studyArea)

  # if there are no raster cells found in the studyArea, exit the function
  if (nrow(studyData) > 0) {
    outList <- list(studyRas = studyData, mapRas = mapData, regionRas = regionData)
    return(outList)

  } # end of test for zero samples
  else {
    return()
  }
}
##### - END raster_intersect function ##################################

##### - create_table_RV function ##################################
# This function creates summary tables of the RV data
# found within the studyArea
#
# Inputs:
# 1. data_sf: an input point file of RV survey data found within the studyArea (eg. output from master_intersect)
# 2. listed_table: a table of species at risk listed by SARA and/or COSEWIC
# 3. speciestable: the RVGSSPECIES species table to link species codes with names
#
# Outputs: list containing 2 items
# 1. allSpeciesData: datatable of all species found within the studyArea
# 2. sarData: datatable of only listed species found within the studyArea

create_table_RV <- function(data_sf, sarTable, speciesTable, ...) {

  # calculate the number of unique sample locations
  Samples_study_no <- dim(unique(data_sf[, c("geometry")]))[1]
  # calculate a table of all species caught and
  # the total number of individuals caught.
  # Join to the species lookup table to get
  # species names
  individualCounts <- aggregate(
    x = list(Individuals = data_sf$TOTNO),
    by = list(CODE = data_sf$CODE),
    FUN = sum)

  recordCounts <- aggregate( 
    x = list(Records = data_sf$CODE),
    by = list(CODE = data_sf$CODE),
    FUN = length)
  allSpeciesData <- merge(individualCounts, recordCounts, by = 'CODE')
  allSpeciesData <- merge(allSpeciesData, speciesTable, by = 'CODE')
  # add a field for the number of samples
  allSpeciesData$Samples <- Samples_study_no
  # combine the number of species records with number of samples
  # into a new field for Frequency
  allSpeciesData <- allSpeciesData %>% tidyr::unite("Frequency", c(Records, Samples),
                                            sep = "/", remove = FALSE)
  
  allSpeciesData <- dplyr::select(allSpeciesData, "Scientific Name", "Common Name",
                              Individuals, Frequency)
  
  # filter allSpeciesData for only SAR species, add status values
  sarData <- filter(allSpeciesData, `Scientific Name` %in% 
                      sarTable$`Scientific Name`)
  # need this select to avoid duplicate "common name" col.
  sarData <- dplyr::select(sarData, "Scientific Name", Individuals, Frequency)
  sarData <- merge(sarData, sarTable, by = 'Scientific Name')
  sarData <- dplyr::select(sarData, "Scientific Name", "Common Name",
                                  "SARA status", "COSEWIC status", Individuals, Frequency)
  
  
  # order the tables by number of individuals caught (decreasing)
  allSpeciesData <- allSpeciesData[with(allSpeciesData, order(-Individuals)), ]
  sarData <- sarData[with(sarData, order(-Individuals)), ]
  row.names(allSpeciesData) <- NULL
  row.names(sarData) <- NULL
  outList <- list(allSpeciesData, sarData)
  return(outList)
}
##### - END create_table_RV function ##################################

##### - create_table_MARFIS function ##################################
# This function creates summary tables of the MARFIS data
# found within the studyArea
#
# Inputs:
# 1. data_sf: an input point file of MARFIS data found within the studyArea
# 2. sarTable: a table of species at risk listed by SARA and/or COSEWIC
# 3. speciesTable: the MARFISSPECIESCODES species table to link species codes with names

#
# Outputs: list containing 2 items
# 1. allSpeciesData: datatable of all species found within the studyArea
# 2. sarData: datatable of only listed species found within the studyArea
create_table_MARFIS <- function(data_sf, sarTable, speciesTable, ...) {

  # set record column and join with speciesTable
  allSpeciesData <- aggregate(
    x = list(Records = data_sf$SPECIES_CODE),
    by = list(SPECIES_CODE = data_sf$SPECIES_CODE),
    FUN = length)
  
  allSpeciesData <- merge(allSpeciesData, speciesTable, by = 'SPECIES_CODE')
  allSpeciesData <- allSpeciesData %>% rename("Common Name"= COMMONNAME)
  
  data1 <- merge(data_sf, speciesTable, by = 'SPECIES_CODE')
  data1$Common_Name_MARFIS <- data1$COMMONNAME

  # Merge the data_sf with the listed_species table
  # and create a frequency table of all listed species
  # caught
  data1 <- merge(data1, sarTable, by = 'Common_Name_MARFIS')
  # data1 <- data1 %>% rename("SCIENTIFICNAME" = Scientific_Name)

  sarData <- aggregate(
    x = list(Records = data1$'Scientific Name'),
    by = list('Scientific Name' = data1$'Scientific Name'),
    length)
  sarData <- merge(sarData, sarTable, by = 'Scientific Name')



  allSpeciesData <- dplyr::select(allSpeciesData, 'Common Name', Records)
  allSpeciesData <- allSpeciesData %>% rename(CName = 'Common Name')
  allSpeciesData <- allSpeciesData %>% transmute(allSpeciesData,
                                                 CName = str_to_sentence(CName))
  allSpeciesData <- allSpeciesData %>% rename('Common Name' = CName)
  sarData <- dplyr::select(sarData, 'Scientific Name', 'Common Name',
                           "SARA status","COSEWIC status", Records)
 
  
   # order the tables by number of Records (decreasing)
  allSpeciesData <- allSpeciesData[with(allSpeciesData, order(-Records)), ]
  sarData <- sarData[with(sarData, order(-Records)), ]
  row.names(allSpeciesData) <- NULL
  row.names(sarData) <- NULL
  outList <- list(allSpeciesData, sarData)
  return(outList)

}
##### - END create_table_MARFIS function ##################################

##### - create_table_ISDB function ##################################
# This function creates summary tables of the ISDB data
# found within the studyArea
#
# Inputs:
# 1. data_sf: an input point file of ISDB data found within the studyArea
# 2. sarTable: a table of species at risk listed by SARA and/or COSEWIC
# 3. speciesTable: the ISSPECIESCODES species table to link species codes with names
#
# Outputs: list containing 2 items
# 1. allSpeciesData: datatable of all species found within the studyArea
# 2. datatable2: datatable of only listed species found within the studyArea

create_table_ISDB <- function(data_sf, sarTable, speciesTable, ...) {

  # calculate frequency of ISDB samples and join
  # to species lookup tables

  allSpeciesData <- aggregate(
    x = list(Records = data_sf$SPECCD_ID),
    by = list(SPECCD_ID = data_sf$SPECCD_ID),
    FUN = length)
  data1 <- merge(data_sf, speciesTable, by = 'SPECCD_ID')
  allSpeciesData <- merge(allSpeciesData, speciesTable, by = 'SPECCD_ID')
  # Merge the data_sf with the listed_species table
  # and create a frequency table of all listed species
  # caught
  data1 <- merge(data1,sarTable, by = 'Scientific Name')

  sarData <- aggregate(
    x = list(Records = data1$'Scientific Name'),
    by = list('Scientific Name' = data1$'Scientific Name'),
    length)
  sarData <- merge(sarData, sarTable, by = 'Scientific Name')


  allSpeciesData <- dplyr::select(allSpeciesData, 'Scientific Name', 'Common Name', Records)
  sarData <- dplyr::select(sarData, 'Scientific Name', 'Common Name',
                           "SARA status","COSEWIC status",Records)
  # order the tables by number of Records (decreasing)
  allSpeciesData <- allSpeciesData[with(allSpeciesData, order(-Records)), ]
  sarData <- sarData[with(sarData, order(-Records)), ]
  row.names(allSpeciesData) <- NULL
  row.names(sarData) <- NULL

  outList <- list(allSpeciesData, sarData)
  return(outList)
}
##### - END create_table_ISDB function ##################################

##### - create_table_OBIS function ##################################
# This function creates a summary table of the OBIS data
# found within the studyArea
# NOTE: the OBIS dataset has already been reduced down to just
# SARA and COSEWIC listed species
#
# Inputs:
# 1. data_sf: an input point file of OBIS data found within the studyArea
#
# Outputs: list containing 1 items
# 1. outTable: datatable of all species found within the studyArea

create_table_OBIS <- function(data_sf, ...) {

  # calculate frequency of OBIS samples
  outTable <- data_sf

  outTable <- dplyr::select(outTable, "Scientific Name", "Common Name",
                              "SARA status","COSEWIC status")
  outTable$geometry <- NULL
  outTable <- unique(outTable)

  row.names(outTable) <- NULL
  outList <- list(outTable)
  return(outList)
}


##### - sfcoords_as_cols function ##################################
# This function extracts X and Y coordinates from the
# sf geometry field and puts them in new fields ("long", "lat")
#
#
# Outputs: returns the datatable with new fields
# for coordinates
sfcoords_as_cols <- function(data_sf, names = c("long","lat")) {
  stopifnot(inherits(data_sf, "sf") && inherits(sf::st_geometry(data_sf),"sfc_POINT"))
  ret <- sf::st_coordinates(data_sf)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  data_sf <- data_sf[ , !names(data_sf) %in% names]
  ret <- setNames(ret, names)
  data_sf <- dplyr::bind_cols(data_sf, ret)
  return(data_sf)
}
##### - END sfcoords_as_cols function ##################################


####### - Other functions (by Greg Puncher) ##########################################
########## Species At Risk distribution and Critical Habitat data ##########
#
# #SAR distribution
table_dist <- function(sardist_sf) {
  sardist_sf$Common_Nam[sardist_sf$Common_Nam == "Sowerby`s Beaked Whale"] <- "Sowerby's Beaked Whale"
  dist_table <-  sardist_sf %>% dplyr::select(Scientific, Common_Nam, Population, SARA_Statu, Species_Li)
  sf::st_geometry(dist_table) <- NULL
  row.names(dist_table) <- NULL
  names(dist_table) <- c("Scientific Name", "Common Name", "Population", "SARA Status", "Species Link")
  return(dist_table)
}

# #SAR critical habitat
table_crit <- function(ClippedCritHab_sf, studyArea, leatherback_sf) {

  intersect_crit <- sf::st_intersection(ClippedCritHab_sf, studyArea)
  intersect_crit_result <- nrow(intersect_crit)
  crit_table <- data.frame(CommonName = intersect_crit$Common_Nam,
                           Population = intersect_crit$Population,
                           Area = intersect_crit$Waterbody,
                           SARA_status = intersect_crit$SARA_Statu)
  
  leatherback_table <- data.frame(CommonName="", Population="", Area="", SARA_status="")
  intersect_leatherback <- sf::st_intersection(leatherback_sf, studyArea)
  leatherback_result <- nrow(intersect_leatherback)
  leatherback_table[1,1] <- "Leatherback Sea Turtle"
  leatherback_table[1,2] <- NA
  if(leatherback_result >= 1){
    # handle case where result > 1
    leatherback_table[1,3] <- paste(intersect_leatherback$AreaName, collapse=', ' )
  }else{
    leatherback_table[1,3] <- NA
  }
  leatherback_table[1,4] <- "Endangered"
  crit_table <- bind_rows(crit_table, leatherback_table)
  crit_table <- crit_table[!is.na(crit_table$Area), ]

  return(crit_table)
}


########## Spatial Planning Section ##########

# # EBSA report
EBSA_report <- function(EBSA_sf, lang="EN") {

  EBSAreport <-  if (lang=="EN" & !is.null(EBSA_sf)) {
    c(paste("Report: ", EBSA_sf$Report),
      paste("Report URL:",EBSA_sf$Report_URL),
      paste("Location: ",EBSA_sf$Name),
      paste("Bioregion: ", EBSA_sf$Bioregion) 
      )
    } else if (lang=="FR" & !is.null(EBSA_sf)) {
      c(paste("Report: ", EBSA_sf$Rapport),
        paste("Report URL:",EBSA_sf$RapportURL),
        paste("Location: ",EBSA_sf$Nom),
        paste("Bioregion: ", EBSA_sf$Bioregion) 
      )
    } else {
      ""
    }
  
  uniqueEBSAreport <- unique(noquote(EBSAreport))
  writeLines(uniqueEBSAreport, sep="\n\n")

}

