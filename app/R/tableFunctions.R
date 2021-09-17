# --------List of table functions-----------
# rockweed stats -three col table with area/status
# create_table_RV
# create_table_MARFIS
# create_table_ISDB
# create_table_OBIS
# table_dist
# table_crit
# EBSA_report
#

# Rockweed stats
rockweedStats<- function(rockweed_sf) {

  # clip rockweed to study area
  rw = sf::st_make_valid(rockweed_sf)
  rw$area = sf::st_area(rw) # add column with areas of the polygons

  # make a table, sum the areas for different presences
  noRecords = as.data.frame(table(rw$status))
  totalArea = aggregate(as.numeric(rw$area), list(rw$status), sum)
  stats = merge(noRecords, totalArea, by.x="Var1", by.y="Group.1")
  
  statusCol <- totalArea$Group.1
  stats <- dplyr::select(stats, c("Freq", "x"))
  stats = rbind(stats, colSums(stats))
  statusCol[nrow(stats)] = "Total intertidal vegetation"
  stats$status <- statusCol
  stats <- dplyr::select(stats, c("status", "Freq", "x"))
  
  names(stats) = c("Status", "noPolygons", "Area_m2")

  stats$Area_km2 = round(stats$Area_m2 / 1000) / 1000
  stats = stats[, c("Status", "noPolygons", "Area_km2")]

  return(stats)

}


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

create_table_RV <- function(data_sf, sarTable, speciesTable) {
  
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
  outList <- list("allSpecies" = allSpeciesData, "sarData" = sarData)
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
                           "SARA status","COSEWIC status", Records)
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

create_table_OBIS <- function(data_sf) {
  
  # calculate frequency of OBIS samples
  outTable <- data_sf
  
  outTable <- dplyr::select(outTable, "Scientific Name", "Common Name",
                            "SARA status","COSEWIC status")
  outTable$geometry <- NULL
  outTable <- unique(outTable)
  
  row.names(outTable) <- NULL
  return(outTable)
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
  dist_table <- dplyr::select(sardist_sf, Scientific, Common_Nam, Population, SARA_Statu, Species_Li)
  sf::st_geometry(dist_table) <- NULL
  row.names(dist_table) <- NULL
  dist_table <- unique(dist_table)
  names(dist_table) <- c("Scientific Name", "Common Name", "Population", "SARA Status", "Species Link")
  return(dist_table)
}

#SAR critical habitat
table_crit <- function(CCH_sf, LB_sf) {
  
  if (!is.null(CCH_sf)){
    critTable <- dplyr::select(CCH_sf, c("Common_Nam", "Population", "Waterbody", "SARA_Statu"))
    critTable$geometry <- NULL
    names(critTable) <- c("CommonName", "Population", "Area", "SARA_status")
  } else {
    critTable <- data.frame("CommonName"=NA, "Population"=NA, "Area"=NA, "SARA_status"=NA)
  }
  
  if (!is.null(LB_sf)){
    leatherbackRow <- data.frame("Leatherback Sea Turtle", NA, paste(LB_sf$AreaName, collapse=', ' ), "Endangered" )
    names(leatherbackRow) <- names(critTable)
    critTable <- bind_rows(critTable, leatherbackRow)
  }
 
  # remove rows with NA area, dump duplicates:
  critTable <- distinct(critTable[!is.na(critTable$Area), ])
  
  if (!nrow(critTable) >= 1){
    return(NULL)
  } else {
    return(critTable)  
  }
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


