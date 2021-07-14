# The functions in this file are used to clip various data sources (e.g. MARFIS, ISDB, etc.)
# with the studyArea selected and summarize the data for tables within the final document.
#
# 1. point_intersect() - clips POINT data to the extent of the studyArea
# 2, poly_intersect() - clips POLYGON data to the extent of the Region and studyArea
# 3. raster_intersect() - clips RASTER data to the extent of the studyArea
# 4. create_table_RV() - creates summary tables of all species and listed species
# 4. create_table_MARFIS() - creates summary tables of all species and listed species
# 6. create_table_ISDB() - creates summary tables of all species and listed species
# 7. create_table_OBIS() - creates summary table of listed species
# 8. sfcoords_as_cols() - extracts latitude and longitude from geometry field
#                         and adds them to new columns
#
# Written by Philip Greyson for Reproducible Reporting project, May/2021



##### - point_intersect function ##################################
# This function clips various point data sources (e.g. MARFIS, ISDB, etc.)
# to the extent of the studyArea and the map bounding box.
# The map bounding box is created by the area_map() function
#
# Inputs:
# 1. datafile: an input point or raster file
# 2. studyArea: polygon of the study area (sf object, defined by the user in the shiny app)
# 3. Bbox: Coordinates of the map bounding box exported from area_map() function
# 4. Year: Minimum year for the data, defined in intro_EN.Rmd as minYear
#
# Outputs: list containing 3 items
# 1. studyData: the full dataset from clipping the datafile by the studyArea
# 2. mapData: the full dataset from clipping the datafile by the Bounding box
#                  used for mapping of the cetacean data points
# 3. mapPoints: set of unique points found within the Bounding box used for mapping



point_intersect <- function(datafile, studyArea, Bbox, Year, ...) {
  
  # Limit data file to data from minYear to present
  # datafile <- datafile %>% dplyr::filter(YEAR >= Year)
  # convert Bbox to sf object
  Bbox <- st_as_sfc(Bbox)
  # clip the data file first to the extent of the bounding box
  # and then clip that reduced datafile to the extent of the 
  # study area
  p1 <- sf::st_crop(datafile,Bbox)
  p2 <- sf::st_crop(p1,studyArea)
  data1 <- p2
  data2 <- p1

  # if there are no samples found in the studyArea, exit the function
  if (nrow(p2) > 0) {
    
    # create smaller files for the points by selecting
    # only the geometry field
    # for RV the ELAT and ELONG fields are necessary
    # for the final mapping
    if ("ELAT" %in% colnames(p2)) {
      p1 <- dplyr::select(p1, ELAT, ELONG, geometry)
    } else {
      p1 <- dplyr::select(p1,geometry)
    }
    # create smaller point files by taking only 
    # unique points
    p1 <- unique(p1)
    # Calculate the number of samples/tows within 
    # the study area
    # Samples_study_no <- nrow(Samples_study)
    
    # add coordinates to table from geometry column
    p1 <- sfcoords_as_cols(p1)
    
    outList <- list(studyData = data1, mapData = data2, mapPoints = p1)
    return(outList)
    
  } # end of test for zero samples
  else {
    return()
  }
}
##### - END point_intersect function ##################################

##### - poly_intersect function ##################################
# This function clips various polygon data sources (e.g. EBSA, critical habitat, etc.)
# to the extent of the region, the studyArea, and the map bounding box.
# The map bounding box is created by the area_map() function
#
# Inputs:
# 1. datafile: an input polygon vector file
# 2. region: a spatial file of the region
# 3. studyArea: polygon of the study area (sf object, defined by the user in the shiny app)
# 4. Bbox: Coordinates of the map bounding box exported from area_map() function (bboxMap)
#
# Outputs: list containing 3 items
# 1. studyPoly: the full dataset from clipping the datafile by the studyArea
# 2. mapPoly: the full dataset from clipping the datafile by the Bounding box
# 3. regionPoly: the full dataset from clipping the datafile by the Region
#
# Created by Phil Greyson June 2021 for reproducible reporting project

poly_intersect <- function(datafile, region, studyArea, Bbox, ...) {

  # convert Bbox to sf object
  Bbox <- st_as_sfc(Bbox)
  # clip the data file first to the extent of the region
  # and then clip that reduced datafile to the extent of the 
  # bounding box, then clip that reduced datafile to the 
  # extent of the studyArea
  p1 <- sf::st_crop(datafile, region)
  p2 <- sf::st_crop(p1,Bbox)
  p3 <- sf::st_crop(p2,studyArea)
  
  # if there are no samples found in the studyArea, exit the function
  if (nrow(p3) > 0) {
    outList <- list(studyPoly = p3, mapPoly = p2, regionPoly = p1)
    return(outList)
    
  } # end of test for zero samples
  else {
    return()
  }
}

##### - END poly_intersect function ##################################

##### - raster_intersect function ##################################
# This function clips various raster data sources (e.g. SDM output, etc.)
# to the extent of the region, the studyArea, and the map bounding box.
# The map bounding box is created by the area_map() function
#
# Inputs:
# 1. datafile: an input raster file
# 2. region: a spatial file of the region
# 3. studyArea: polygon of the study area (sf object, defined by the user in the shiny app)
# 4. Bbox: Coordinates of the map bounding box exported from area_map() function
#
# Outputs: list containing 3 items
# 1. studyRas: the full dataset from clipping the datafile by the studyArea
# 2. mapRas: the full dataset from clipping the datafile by the Bounding box
# 3. regionRas: the full dataset from clipping the datafile by the Region
raster_intersect <- function(datafile, region, studyArea, Bbox, ...) {
  
  # convert Bbox to sf object
  Bbox <- st_as_sfc(Bbox)
  Bbox <- st_as_sf(Bbox)
  # clip the data file first to the extent of the region
  # and then clip that reduced datafile to the extent of the 
  # bounding box, then clip that reduced datafile to the 
  # extent of the studyArea
  # with raster datasets it's necesssary to comnbine the
  # crop and mask functions
  p1 <- crop(datafile, region)
  p1 <- raster::mask(p1, region)
  p2 <- crop(p1, Bbox)
  p2 <- mask(p2, Bbox)
  p3 <- crop(p2, studyArea)
  p3 <- mask(p3, studyArea)
  
  # if there are no raster cells found in the studyArea, exit the function
  if (nrow(p3) > 0) {
    outList <- list(studyRas = p3, mapRas = p2, regionRas = p1)
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
# 1. datafile: an input point file of RV survey data found within the studyArea
# 2. listed_table: a table of species at risk listed by SARA and/or COSEWIC
# 3. speciestable: the RVGSSPECIES species table to link species codes with names
# 4. Samples_study_no: output from main_intersect() function of the number of samples in studyArea
#
# Outputs: list containing 2 items
# 1. datatable1: datatable of all species found within the studyArea
# 2. datatable2: datatable of only listed species found within the studyArea

create_table_RV <- function(datafile, listed_table, speciestable, ...) {
  
  # calculate the number of unique sample locations
  Samples_study_no <- dim(unique(datafile[,c("geometry")]))[1]
  # calculate a table of all species caught and 
  # the total number of individuals caught.
  # Join to the species lookup table to get 
  # species names
  individuals <- aggregate(
    x = list(Individuals = datafile$TOTNO),
    by = list(CODE = datafile$CODE),
    FUN = sum)
  
  datatable1 <- aggregate(
    x = list(Records = datafile$CODE),
    by = list(CODE = datafile$CODE),
    FUN = length)
  datatable1 <- merge(individuals,datatable1, by = 'CODE')
  
  data1 <- merge(datafile,speciestable, by = 'CODE')
  datatable1 <- merge(datatable1,speciestable, by = 'CODE')
  
  # Merge the datafile with the listed_species table
  # and create a frequency table of all listed species 
  # caught
  data1 <- merge(data1,listed_table, by = 'Scientific Name')
  datatable2 <- aggregate(
    x = list(Records = data1$'Scientific Name'),
    by = list('Scientific Name' = data1$'Scientific Name'),
    length)
  # merge the frequency table with listed_table to get
  # the SARA and COSEWIC listings for each species
  datatable2 <- merge(datatable2,listed_table, by = 'Scientific Name')
  # add a field for the number of samples
  datatable1$Samples <- Samples_study_no
  # combine the number of species records with number of samples
  # into a new field for Frequency
  datatable1 <- datatable1 %>% tidyr::unite("Frequency", c(Records,Samples), 
                                            sep = "/", remove = FALSE)
  
  datatable1 <- dplyr::select(datatable1, "Scientific Name", "Common Name",
                              Individuals,Frequency)
  datatable2 <- merge(datatable2,datatable1, by = 'Scientific Name')
  datatable2 <- dplyr::select(datatable2, "Scientific Name", Individuals,Frequency)
  datatable2 <- merge(datatable2,listed_table, by = 'Scientific Name')
  datatable2 <- dplyr::select(datatable2, "Scientific Name", "Common Name", 
                              "SARA status","COSEWIC listing",Individuals,Frequency)
  
  # order the tables by number of individuals caught (decreasing)
  datatable1 <- datatable1[with(datatable1, order(-Individuals)), ]
  datatable2 <- datatable2[with(datatable2, order(-Individuals)), ]
  row.names(datatable1) <- NULL
  row.names(datatable2) <- NULL
  outList <- list(datatable1, datatable2)
  return(outList)
}
##### - END create_table_RV function ##################################

##### - create_table_MARFIS function ##################################
# This function creates summary tables of the MARFIS data
# found within the studyArea
#
# Inputs:
# 1. datafile: an input point file of MARFIS data found within the studyArea
# 2. listed_table: a table of species at risk listed by SARA and/or COSEWIC
# 3. speciestable: the MARFISSPECIESCODES species table to link species codes with names

#
# Outputs: list containing 2 items
# 1. datatable1: datatable of all species found within the studyArea
# 2. datatable2: datatable of only listed species found within the studyArea
create_table_MARFIS <- function(datafile, listed_table, speciestable, ...) {
  
  # calculate frequency of MARFIS samples and join 
  # to species lookup tables
  datatable1 <- aggregate(
    x = list(Records = datafile$SPECIES_CODE),
    by = list(SPECIES_CODE = datafile$SPECIES_CODE),
    FUN = length)
  datatable1 <- merge(datatable1,speciestable, by = 'SPECIES_CODE')
  datatable1 <- datatable1 %>% rename("Common Name"= COMMONNAME)
  data1 <- merge(datafile,speciestable, by = 'SPECIES_CODE')
  data1$Common_Name_MARFIS <- data1$COMMONNAME
  
  # Merge the datafile with the listed_species table
  # and create a frequency table of all listed species 
  # caught
  data1 <- merge(data1,listed_table, by = 'Common_Name_MARFIS')
  # data1 <- data1 %>% rename("SCIENTIFICNAME" = Scientific_Name)
  
  datatable2 <- aggregate(
    x = list(Records = data1$'Scientific Name'),
    by = list('Scientific Name' = data1$'Scientific Name'),
    length)
  datatable2 <- merge(datatable2,listed_table, by = 'Scientific Name')
  
  
  
  datatable1 <- dplyr::select(datatable1, 'Common Name', Records)
  datatable1 <- datatable1 %>% rename(CName = 'Common Name')
  datatable1 <- datatable1 %>% transmute(datatable1, CName = str_to_sentence(CName))
  datatable1 <- datatable1 %>% rename('Common Name' = CName)
  datatable2 <- dplyr::select(datatable2, 'Scientific Name', 'Common Name',
                              "SARA status","COSEWIC listing",Records)
  # order the tables by number of Records (decreasing)
  datatable1 <- datatable1[with(datatable1, order(-Records)), ]
  datatable2 <- datatable2[with(datatable2, order(-Records)), ]
  row.names(datatable1) <- NULL
  row.names(datatable2) <- NULL
  outList <- list(datatable1, datatable2)
  return(outList)
  
}
##### - END create_table_MARFIS function ##################################

##### - create_table_ISDB function ##################################
# This function creates summary tables of the ISDB data
# found within the studyArea
#
# Inputs:
# 1. datafile: an input point file of ISDB data found within the studyArea
# 2. listed_table: a table of species at risk listed by SARA and/or COSEWIC
# 3. speciestable: the ISSPECIESCODES species table to link species codes with names
#
# Outputs: list containing 2 items
# 1. datatable1: datatable of all species found within the studyArea
# 2. datatable2: datatable of only listed species found within the studyArea

create_table_ISDB <- function(datafile, listed_table, speciestable, ...) {
  
  # calculate frequency of ISDB samples and join 
  # to species lookup tables
  
  datatable1 <- aggregate(
    x = list(Records = datafile$SPECCD_ID),
    by = list(SPECCD_ID = datafile$SPECCD_ID),
    FUN = length)
  data1 <- merge(datafile,speciestable, by = 'SPECCD_ID')
  datatable1 <- merge(datatable1,speciestable, by = 'SPECCD_ID')
  # Merge the datafile with the listed_species table
  # and create a frequency table of all listed species 
  # caught
  data1 <- merge(data1,listed_table, by = 'Scientific Name')
  
  datatable2 <- aggregate(
    x = list(Records = data1$'Scientific Name'),
    by = list('Scientific Name' = data1$'Scientific Name'),
    length)
  datatable2 <- merge(datatable2,listed_table, by = 'Scientific Name')
  
  
  datatable1 <- dplyr::select(datatable1, 'Scientific Name', 'Common Name', Records)
  datatable2 <- dplyr::select(datatable2, 'Scientific Name', 'Common Name', 
                              "SARA status","COSEWIC listing",Records)
  # order the tables by number of Records (decreasing)
  datatable1 <- datatable1[with(datatable1, order(-Records)), ]
  datatable2 <- datatable2[with(datatable2, order(-Records)), ]
  row.names(datatable1) <- NULL
  row.names(datatable2) <- NULL
  
  outList <- list(datatable1, datatable2)
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
# 1. datafile: an input point file of OBIS data found within the studyArea
#
# Outputs: list containing 1 items
# 1. datatable1: datatable of all species found within the studyArea

create_table_OBIS <- function(datafile, ...) {
  
  # calculate frequency of OBIS samples
  datatable1 <- datafile
  # datatable1 <- datatable1 %>% rename("COSEWIC listing"=COSEWIC.listing, 
  #                                     "SARA status"=SARA.status,
  #                                     "Scientific Name"=Scientific.Name,
  #                                     "Common Name"=Common.Name)
  
  datatable1 <- dplyr::select(datatable1, "Scientific Name", "Common Name", 
                              "SARA status","COSEWIC listing")
  datatable1$geometry <- NULL
  datatable1 <- unique(datatable1)
  
  row.names(datatable1) <- NULL
  outList <- list(datatable1)
  return(outList)
}
##### - END create_table_OBIS function ##################################

##### - sfcoords_as_cols function ##################################
# This function extracts X and Y coordinates from the 
# sf geometry field and puts them in new fields ("long", "lat")
# 
#
# Outputs: returns the datatable with new fields
# for coordinates
sfcoords_as_cols <- function(x, names = c("long","lat")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  x <- dplyr::bind_cols(x,ret)
  return(x)
}
##### - END sfcoords_as_cols function ##################################


####### - Other functions (by Greg Puncher) ##########################################
########## Species At Risk distribution and Critical Habitat data ##########
# 
# #SAR distribution
table_dist <- function(sardist_sf, studyArea) {
  #table_dist <- function(sardist_sf, studyArea, listed_species) {
  intersect_dist <- sf::st_intersection(sardist_sf,studyArea)
  intersect_dist$Common_Nam[intersect_dist$Common_Nam == "Sowerby`s Beaked Whale"] <- "Sowerby's Beaked Whale"
  dist_table <- intersect_dist %>% dplyr::select(Scientific, Common_Nam, Population, SARA_Statu, Species_Li)
  #dist_table <- dist_table %>% dplyr::rename("Scientific_Name"=Scientific)
  #dist_table <- merge(dist_table, listed_species, by='Scientific_Name')
  #dist_table <- dist_table %>% dplyr::select(Common_Nam, Scientific, Population, Waterbody, Schedule.status, COSEWIC.status, Wild_Species)
  sf::st_geometry(dist_table) <- NULL
  #dist_table <- dist_table %>% dplyr::rename("SARA status"=Schedule.status,
  # "COSEWIC listing"=COSEWIC.status,
  # "Wild Species listing"=Wild_Species,
  # "Common Name"=Common_Name,
  # "Scientific Name"=Scientific_Name)
  row.names(dist_table) <- NULL
  return(dist_table)
}

# #SAR critical habitat
table_crit <- function(ClippedCritHab_sf, studyArea, leatherback_sf) {
  
  intersect_crit <- sf::st_intersection(ClippedCritHab_sf,studyArea)
  intersect_crit_result <- nrow(intersect_crit)
  crit_table <- data.frame(CommonName=intersect_crit$Common_Nam,
                           Population=intersect_crit$Population, 
                           Area=intersect_crit$Waterbody,
                           SARA_status=intersect_crit$SARA_Statu)
  leatherback_table <- data.frame(CommonName="",Population="", Area="", SARA_status="")
  intersect_leatherback <- sf::st_intersection(leatherback_sf,studyArea)
  leatherback_result <- nrow(intersect_leatherback)
  leatherback_table[1,1] <- "Leatherback Sea Turtle"
  leatherback_table[1,2] <- NA
  if(leatherback_result >= 1){
    leatherback_table[1,3] <- intersect_leatherback$AreaName
  }else{
    leatherback_table[1,3] <- NA  
  }
  leatherback_table[1,4] <- "Endangered"
  crit_table <- bind_rows(crit_table,leatherback_table)
  crit_table <- crit_table[!is.na(crit_table$Area), ]
  
  return(crit_table)
}


# #Species Distribution Models (SDM): Priority Areas to Enhance Monitoring of Cetaceans
sdm_table <- function(fin_whale_sf, harbour_porpoise_sf, humpback_whale_sf, sei_whale_sf, studyArea) {
  
  fin_intersect <- sf::st_intersection(fin_whale_sf,studyArea)
  x<-as.numeric(nrow(fin_intersect))
  fin_area<-if(x < 1){
    FALSE
  } else {
    TRUE
  }
  
  harbour_intersect <- sf::st_intersection(harbour_porpoise_sf,studyArea)
  x<-as.numeric(nrow(harbour_intersect))
  harbour_area<-if(x < 1){
    FALSE
  } else {
    TRUE
  }
  
  humpback_intersect <- sf::st_intersection(humpback_whale_sf,studyArea)
  x<-as.numeric(nrow(humpback_intersect))
  humpback_area<-if(x < 1){
    FALSE
  } else {
    TRUE
  }
  
  sei_intersect <- sf::st_intersection(sei_whale_sf,studyArea)
  x<-as.numeric(nrow(sei_intersect))
  sei_area<-if(x < 1){
    FALSE
  } else {
    TRUE
  }
  
  table_sdm<-data.frame(Fin_Whale="",Habour_Porpoise="", Humpback_Whale="", Sei_Whale="")
  table_sdm[1,1]<-fin_area
  table_sdm[1,2]<-harbour_area
  table_sdm[1,3]<-humpback_area
  table_sdm[1,4]<-sei_area
  table_sdm<- table_sdm %>% dplyr::rename("Fin Whale"=Fin_Whale,
                                          "Habour Porpoise"=Habour_Porpoise,
                                          "Humpback Whale"=Humpback_Whale,
                                          "Sei Whale"=Sei_Whale)
  return(table_sdm)
  
}

# #Blue Whale Important Habitat
blue_whale_habitat_overlap <- function(Blue_Whale_sf, studyArea) {
  
  intersect <- sf::st_intersection(Blue_Whale_sf,studyArea)
  x <- as.numeric(nrow(intersect))
  Query_output_crit <- if(x < 1){
    "Search area does not overlaps with Blue Whale Important Habitat in the Western North Atlantic."
  } else {
    "Search area overlaps with Blue Whale Important Habitat in the Western North Atlantic."
  }
  
}

# Northern Bottlenose Whale critical habitat
intersect_NBNW_overlap <- function(NBNW, studyArea) {
  intersect <- sf::st_intersection(NBNW,studyArea)
  y <- as.numeric(nrow(intersect))
  Query_output_criteria <- if(y < 1){
    "Search area does not overlap with Northern Bottlenose Whale Important Habitat in the Western North Atlantic."
  } else {
    "Search area overlaps with Northern Bottlenose Whale Important Habitat in the Western North Atlantic."
  }
}

########## Spatial Planning Section ##########

# #Ecologically and Biologically Significant Areas (EBSA)
EBSA_overlap <- function(EBSA_sf, studyArea) {
  
  EBSA_intersect <- sf::st_intersection(EBSA_sf,studyArea)
  EBSA_result<-as.numeric(nrow(EBSA_intersect))
  Query_output_EBSA<-if(EBSA_result < 1){
    "The search area does not overlap with identified Ecologically and Biologically Significant Areas (EBSA)."
  } else {
    "The search area overlaps with identified Ecologically and Biologically Significant Areas (EBSA)."
  }
  
  Query_output_EBSA2<-noquote(Query_output_EBSA)
  
  writeLines(Query_output_EBSA2)
  
}

# # EBSA report
EBSA_report <- function(EBSA_sf, studyArea) {
  
  EBSA_intersect <- sf::st_intersection(EBSA_sf,studyArea)
  EBSA_result<-as.numeric(nrow(EBSA_intersect))
  Query_output_EBSA_report<-if(EBSA_result < 1){
    ""
  } else {
    paste("Report: ", EBSA_intersect$Report)
  }
  
  Query_output_EBSA_report2<-unique(noquote(Query_output_EBSA_report))
  
  writeLines(Query_output_EBSA_report2, sep="\n")
  
}

# # EBSA report URL
EBSA_reporturl <- function(EBSA_sf, studyArea) {
  
  intersect <- sf::st_intersection(EBSA_sf,studyArea)
  x<-as.numeric(nrow(intersect))
  Query_output_EBSA_reporturl<-if(x < 1){
    ""
  } else {
    paste("Report URL:",intersect$Report_URL)
  }
  
  Query_output_EBSA_reporturl2<-unique(noquote(Query_output_EBSA_reporturl))
  
  writeLines(Query_output_EBSA_reporturl2, sep="\n")
  
}

# # Location intersect
EBSA_location <- function(EBSA_sf, studyArea) {
  
  intersect <- sf::st_intersection(EBSA_sf,studyArea)
  x<-as.numeric(nrow(intersect))
  Location_result<-if(x < 1){
    ""
  } else {
    paste("Location: ",intersect$Name)
  }
  
  writeLines(Location_result, sep="\n")
}

#Bioregion intersect
EBSA_bioregion <- function(EBSA_sf, studyArea) {
  
  intersect <- sf::st_intersection(EBSA_sf,studyArea)
  x<-as.numeric(nrow(intersect))
  Query_output_area<-if(x < 1){
    ""
  } else {
    paste("Bioregion: ",intersect$Bioregion)
  }
  
  Query_output_area2<-paste(unique(Query_output_area), collapse = ' ')
  Query_output_area3<-noquote(Query_output_area2)
  
  Bioregion_result<-if(x < 1){
    ""
  } else {
    writeLines(Query_output_area3, sep="\n")
  }
  
  
}


####### - END Other functions ###############################################