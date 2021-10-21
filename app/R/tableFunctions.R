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
# Inputs:
# rockweed_sf: clipped rockweed_sf data
#
# Outputs:
# stats: three column table used in the report.
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
# 2. sarTable: a table of species at risk listed by SARA and/or COSEWIC
# 3. speciesTable: the RVGSSPECIES species table to link species codes with names
#
# Outputs: list containing 2 items
# 1. allSpeciesData: datatable of all species found within the studyArea
# 2. sarData: datatable of only listed species found within the studyArea

create_table_RV <- function(data_sf, sarTable, speciesTable) {

  if (is.null(data_sf)) {
    return(list("allSpecies" = NULL, "sarData" = NULL))
  }

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

  allSpeciesData$`Scientific Name` <- italicize_col(allSpeciesData$`Scientific Name`)
  sarData$`Scientific Name` <- italicize_col(sarData$`Scientific Name`)

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

  if (is.null(data_sf)) {
    return(list("allSpecies" = NULL, "sarData" = NULL))
  }

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

  allSpeciesData$`Scientific Name` <- italicize_col(allSpeciesData$`Scientific Name`)
  sarData$`Scientific Name` <- italicize_col(sarData$`Scientific Name`)

  # order the tables by number of Records (decreasing)
  allSpeciesData <- allSpeciesData[with(allSpeciesData, order(-Records)), ]
  sarData <- sarData[with(sarData, order(-Records)), ]
  row.names(allSpeciesData) <- NULL
  row.names(sarData) <- NULL
  outList <- list("allSpeciesData" = allSpeciesData, "sarData" = sarData)
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

  if (is.null(data_sf)) {
    return(list("allSpecies" = NULL, "sarData" = NULL))
  }

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
  data1 <- merge(data1, sarTable, by = 'Scientific Name')

  sarData <- aggregate(
    x = list(Records = data1$'Scientific Name'),
    by = list('Scientific Name' = data1$'Scientific Name'),
    length)
  sarData <- merge(sarData, sarTable, by = 'Scientific Name')


  allSpeciesData <- dplyr::select(allSpeciesData, 'Scientific Name', 'Common Name', Records)
  sarData <- dplyr::select(sarData, 'Scientific Name', 'Common Name',
                           "SARA status","COSEWIC status", Records)

  allSpeciesData$`Scientific Name` <- italicize_col(allSpeciesData$`Scientific Name`)
  sarData$`Scientific Name` <- italicize_col(sarData$`Scientific Name`)

  # order the tables by number of Records (decreasing)
  allSpeciesData <- allSpeciesData[with(allSpeciesData, order(-Records)), ]
  sarData <- sarData[with(sarData, order(-Records)), ]
  row.names(allSpeciesData) <- NULL
  row.names(sarData) <- NULL

  outList <- list("allSpeciesData" = allSpeciesData, "sarData" = sarData)
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

  if (is.null(data_sf)) {
    return(NULL)
  }

  # calculate frequency of OBIS samples
  outTable <- data_sf

  outTable <- dplyr::select(outTable, "Scientific Name", "Common Name",
                            "SARA status","COSEWIC status")
  outTable$geometry <- NULL
  outTable <- unique(outTable)

  outTable$`Scientific Name` <- italicize_col(outTable$`Scientific Name`)

  row.names(outTable) <- NULL
  return(outTable)
}


##### - sfcoords_as_cols function ##################################
# This function extracts X and Y coordinates from the
# sf geometry field and puts them in new fields ("long", "lat")
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

# ---------TABLE DIST-------
# Generates table for SAR distribution data
# Inputs:
# clippedSardist_sf: Sardist data clipped to area of interest
#
# Outputs:
# distTable: table used in the report
#
table_dist <- function(clippedSardist_sf, lang) {

  if (is.null(clippedSardist_sf)) {
    return(NULL)
  }

  if (lang == "EN") {
    clippedSardist_sf$Common_Name_EN[clippedSardist_sf$Common_Name_EN == "Sowerby`s Beaked Whale"] <- "Sowerby's Beaked Whale"
    distTable <- dplyr::select(clippedSardist_sf, Scientific_Name, Common_Name_EN,
                               Population_EN, SARA_Status, Species_Link)
    tableNames <-  c("Scientific Name", "Common Name", "Population", "SARA Status", "Species Link")
  } else if(lang =="FR") {
    distTable <- dplyr::select(clippedSardist_sf, Scientific_Name, Common_Name_FR,
                               Population_FR, SARA_Status, Species_Link)
    tableNames <-  c("Nom Scientific", "Nom Commun", "Population", "Statut LEP", "Lien d'espèce")
  } else {
    stop("Specify language choice (EN/FR)")
  }

  sf::st_geometry(distTable) <- NULL
  row.names(distTable) <- NULL
  distTable <- unique(distTable)
  distTable$Scientific_Name <- italicize_col(distTable$Scientific)
  names(distTable) <- tableNames

  return(distTable)
}


# helper function that italicizes a column of a table in RMD.
italicize_col <- function(tableCol) {
  if (length(tableCol > 0)) {
    return(paste("_", tableCol, "_", sep = ""))
  } else {
    return(NULL)
  }
}


#SAR critical habitat
# ---------TABLE CRIT-------
# Generates table for SAR SAR critical habitat data
# Inputs:
# CCH_sf: Critical habitat data clipped to area of interest
# LB_sf: Leatherback data clipped to area of interest
#
# Outputs:
# critTable: table used in the report
#
table_crit <- function(CCH_sf, LB_sf, lang) {

  if (lang == "EN"){
    critTableCols <- c("Common_Name_EN", "Population_EN", "Waterbody", "SARA_Status")
    critTableNames <- c("Common Name", "Population", "Area", "SARA status")
    leatherbackRow <- data.frame("Leatherback Sea Turtle", NA, paste(LB_sf$AreaName, collapse=', ' ), "Endangered" )
  } else if (lang =="FR") {
    critTableCols <- c("Common_Name_FR", "Population_FR", "Waterbody", "SARA_Status")
    critTableNames <- c("Nom Commun", "Population", "Region", "Statut LEP")
    leatherbackRow <- data.frame("Tortue Luth", NA, paste(LB_sf$AreaName, collapse=', ' ), "En voie de disparition" )
  } else {
    stop("Specify Critical Habitat Table language choice (EN/FR)")
  }
  
  if (!is.null(CCH_sf)){
    critTable <- dplyr::select(CCH_sf, critTableCols)
    critTable <- sf::st_drop_geometry(critTable)
    names(critTable) <- critTableNames
  } else {
    # only set names after init to preserve spaces etc.
    critTable <- data.frame("a"=NA, "b"=NA, "c"=NA, "d"=NA)
    names(critTable) <- critTableNames
  }

  if (!is.null(LB_sf)){
    names(leatherbackRow) <- critTableNames
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

# ---------EBSA_report-------
# Generates table for SAR SAR critical habitat data
# Inputs:
# EBSA_sf: EBSA data clipped to area of interest
# lang: "EN" or "FR", toggles language of data returned
#
# Outputs:
# Directly writes table
#
EBSA_report <- function(EBSA_sf, lang="EN") {
  EBSATable <- NULL
  if (lang=="EN" & !is.null(EBSA_sf)) {
    EBSATable <- st_drop_geometry(dplyr::select(EBSA_sf, c(Report, Report_URL,
                                                           Name, Bioregion)))
    EBSATable <- unique(EBSATable)
    row.names(EBSATable) <- NULL
    names(EBSATable) <- c("Report", "Report URL", "Location", "Bioreigon")
  } else if (lang=="FR" & !is.null(EBSA_sf)) {
    EBSATable <- st_drop_geometry(dplyr::select(EBSA_sf, c(Rapport, RapportURL,
                                                           Nom, Bioregion)))
    EBSATable <- unique(EBSATable)
    names(EBSATable) <- c("Report", "Report URL", "Location", "Bioreigon")
    row.names(EBSATable) <- NULL

  }
  return(EBSATable)
}

# ---------MPA_REPPRT-------
# Generates table for Marine protected area (MPA) data
# Inputs:
# mpa_sf: MPA data clipped to area of interest
# lang: "EN" or "FR", toggles language of data returned
#
# Outputs:
#  mpaTable
#
mpa_table <- function(mpa_sf, lang="EN") {
  mpa_sf <- dplyr::arrange(mpa_sf, Id)
  # &nbsp; is a non breaking space which is needed to escape the hyphens in kable in shiny AND rmd
  mpa_sf$Id <- gsub(0, "&nbsp;-&nbsp;", mpa_sf$Id)
  mpaTable <- dplyr::select(mpa_sf, c("Id", "NAME", "Legend", "AreaKM2"))
  mpaTable$AreaKM2 <- as.integer(mpaTable$AreaKM2)
  mpaTable$geometry <- NULL


  if (lang=="EN" & !is.null(mpa_sf)) {
    names(mpaTable) <- c("Site #", "Site Name", "Satus", "Size (km2)")
  } else if (lang=="FR" & !is.null(mpa_sf)) {
    names(mpaTable) <- c("Site #", "Site Name", "Satus", "Size (km2)")
  } else {
    names(mpaTable) <- c("Site #", "Site Name", "Satus", "Size (km2)")
  }
  mpaTable <- distinct(mpaTable)
  return(mpaTable)
}

# ---------add_col_to_whale_summary-------
# Adds a column with number of records to the cetacean summary table
# Inputs:
# whaleSummary: Table generated in cetacean setup chunk. Contains a "Species" column.
# dbName: Name of column header to add to summary table
# data_sf: cetacean sf object clipped to study area
# attribute: column header of column in data_sf with species names matching whaleSummary column
#
# Outputs:
# whaleSummary: updated whaleSummary with added column
#
add_col_to_whale_summary <- function(whaleSummary, dbName, data_sf, attribute) {
  if (!is.null(data_sf)){
    data_sf$summaryCol <- data_sf[[attribute]]
    data_sf <- st_drop_geometry(data_sf)
    data_sf <-data_sf %>% dplyr::select(summaryCol) %>%
      group_by(summaryCol) %>%
      summarise(noRecords = length(summaryCol))
  } else {
    data_sf <- whaleSummary
    data_sf$summaryCol <- data_sf$Species
    data_sf[["noRecords"]] <- rep(0, nrow(whaleSummary))
  }

  whaleSummary[[dbName]] <- merge(whaleSummary, data_sf, by.x="Species", by.y ="summaryCol", all=TRUE)$noRecords
  whaleSummary[is.na(whaleSummary)] <- 0
  return(whaleSummary)
}


# ---------add_col_to_sar_summary-------
# Adds a column with presence/absence to the SAR summary table
# Inputs:
# sarSummary: Table generated in intro setup chunk. Contains a "Species" column.
# dbName: Name of column header to add to summary table
# data_sf: sf object clipped to study area
# indexCol: column header of column in data_sf with species names matching sarSummary column
# attributeCol: column header of column in data_sf with species presence/absence
#               matching sarSummary column. Can also be set to indexCol if not present.
#
# Outputs:
# sarSummary: updated sarSummary with added column
#
add_col_to_sar_summary <- function(sarSummary, dbName, dataTable, indexCol, attributeCol) {
  absentCode <- "&nbsp;-&nbsp;"
  presentCode <- "&#x2714;"

  if (!is.null(dataTable)){
    if (indexCol == attributeCol) {
      dataTable <- distinct(dataTable, !!sym(indexCol))
    }
    dataTable$summaryCol <-ifelse(dataTable[[attributeCol]] > 0, presentCode, absentCode)
    dataTable$speciesCol <- dataTable[[indexCol]]
    dataTable <- filter(dataTable, speciesCol %in% sarSummary$Species)
  } else {
    dataTable <- sarSummary
    dataTable$speciesCol <- dataTable$Species
    dataTable$summaryCol <- rep(absentCode, nrow(sarSummary))
  }

  sarSummary[[dbName]] <- merge(sarSummary, dataTable, by.x="Species", by.y ="speciesCol", all=TRUE)$summaryCol
  return(sarSummary)
}
# ---------add_to_hab_summary-------
# Adds entries to a column with dbname in each cell.
# Inputs:
# summaryTable: One row table generated in intro setup chunk.
# dbName: Name of column header to add to summary table
# present: boolean indicating whether or not data was found in database for this table
# Outputs:
# summaryTable: updated summaryTable with added column
#
add_to_hab_summary <- function(summaryTable, colName, dbName, dataTable, indexCol, attributeCol) {

  if (!is.null(dataTable)){
    if (indexCol == attributeCol) {
      dataTable <- distinct(dataTable, !!sym(indexCol))
    }
    dataTable$summaryCol <-ifelse(dataTable[[attributeCol]] > 0, dbName, NA)
    dataTable$speciesCol <- dataTable[[indexCol]]
    dataTable <- filter(dataTable, speciesCol %in% summaryTable$Species)
    dataTable <- filter(dataTable, lengths(summaryCol) > 0)
    tempCol <- merge(summaryTable, dataTable, by.x="Species", by.y ="speciesCol", all=TRUE)$summaryCol

    # nested ifelse to set column value to either new value if not NA, or
    # combination of old and new if both were present
    summaryTable[[colName]] <- ifelse(!(tempCol %in% c(NA)),
                                      ifelse(summaryTable[[colName]] %in% c(NA),
                                             tempCol,
                                             paste(summaryTable[[colName]],
                                                   tempCol, sep = ", ")),
                                      summaryTable[[colName]])
  }
  return(summaryTable)
}

#helper function to trim whale legends down to common name
get_cetacean_common_name <- function(dataCol) {
 return(sub("\\:.*", "", dataCol))
}
