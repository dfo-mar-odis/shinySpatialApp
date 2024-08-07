library(sf)


# --------------Rockweed Table-------------
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


##### - create_table_RV ##################################
# This function creates summary tables of the RV data
# found within the studyArea
#
# Inputs:
# 1. data_sf: an input point file of RV survey data found within the studyArea (eg. output from master_intersect)
# 2. sarTable: a table of species at risk listed by SARA and/or COSEWIC
#
# Outputs: list containing 2 items
# 1. allSpeciesData: datatable of all species found within the studyArea
# 2. sarData: datatable of only listed species found within the studyArea

create_table_RV <- function(data_sf, sarTable) {

  if (is.null(data_sf)) {
    return(list("allSpecies" = NULL, "sarData" = NULL))
  }

  # calculate the number of unique sample locations
  Samples_study_no <- dim(unique(data_sf[, c("geometry")]))[1]
  # calculate a table of all species caught and
  # the total number of individuals caught.
  individualCounts <- aggregate(
    x = list(Individuals = data_sf$TOTNO),
    by = list(CODE = data_sf$CODE, "Scientific Name" = data_sf$`Scientific Name`,
              "Common Name" = data_sf$`Common Name`),
    FUN = sum)

  recordCounts <- aggregate(
    x = list(Records = data_sf$CODE),
    by = list(CODE = data_sf$CODE),
    FUN = length)

  allSpeciesData <- merge(individualCounts, recordCounts, by = 'CODE')
  # add a field for the number of samples
  allSpeciesData$Samples <- Samples_study_no
  # combine the number of species records with number of samples
  # into a new field for Frequency
  allSpeciesData <- allSpeciesData %>% tidyr::unite("Frequency", c(Records, Samples),
                                                    sep = "/", remove = FALSE)

  allSpeciesData <- dplyr::select(allSpeciesData, "Scientific Name", "Common Name",
                                  Individuals, Frequency)

  # filter allSpeciesData for only SAR species, add status values
  sarData <- dplyr::filter(allSpeciesData, `Scientific Name` %in%
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

##### - create_sar_tables ##################################
# This function creates two summary tables of the species data
# found within the studyArea
#
# Inputs:
# 1. data_sf: an input point file of ILTS survey data found within the studyArea (eg. output from master_intersect)
# 2. sarTable: a table of species at risk listed by SARA and/or COSEWIC
# 3. uniqueCols: a list of the unique columns used for frequency counts,
#    eg c("geometry", "year") will count sampling year at each site independently
# 4. extraCols: a list of additional columns to include in the output, allows removal of frequency column
#
# Outputs: list containing 2 items
# 1. allSpeciesData: datatable of all species found within the studyArea
# 2. sarData: datatable of only listed species found within the studyArea

create_sar_tables <- function(data_sf, sarTable, uniqueCols = c("geometry"), extraCols = c("Frequency")) {

  if (is.null(data_sf)) {
    return(list("allSpecies" = NULL, "sarData" = NULL))
  }

  allSpeciesData <- dplyr::select(data_sf, any_of(c("Scientific Name", "Common Name", extraCols)))
  allSpeciesData$geometry <- NULL
  allSpeciesData <- unique(allSpeciesData)

  if ("Frequency" %in% extraCols) {
    # calculate the number of unique sample locations
    numTrawls <- dim(unique(data_sf[, uniqueCols]))[1]
    recordCounts <- aggregate(
      x = list(Records = data_sf$`Scientific Name`),
      by = list("Scientific Name" = data_sf$`Scientific Name`),
      FUN = length)
    allSpeciesData <- merge(allSpeciesData, recordCounts, by = 'Scientific Name')

    # add a field for the number of samples
    allSpeciesData$numTrawls <- numTrawls
    # combine the number of species records with number of samples
    # into a new field for Frequency
    allSpeciesData <- tidyr::unite(allSpeciesData, "Frequency",
                                   c(Records, numTrawls), sep = "/",
                                   remove = FALSE)
    allSpeciesData <- allSpeciesData[order(allSpeciesData$Records, decreasing = TRUE),]
  } else {
    allSpeciesData <- allSpeciesData[order(allSpeciesData$`Common Name`, decreasing = FALSE),]
  }



  allSpeciesData <- dplyr::select(allSpeciesData, c("Scientific Name", "Common Name", all_of(extraCols)))

  skateRow <- dplyr::filter(sarTable, COMMONNAME == "WINTER SKATE")
  skateRow$`Scientific Name` <- "Leucoraja ocellata	(Uncertain)"
  skateRow$`Common Name` <- "Winter Skate (possible Little Skate)"
  sarTable <- rbind(sarTable, skateRow)

  sarData <- dplyr::inner_join(allSpeciesData, sarTable, by="Scientific Name", suffix = c(".x", ""))
  sarData <- dplyr::select(sarData, c("Scientific Name", "Common Name", "SARA status", "COSEWIC status", all_of(extraCols)))
  names(sarData) <- c("Scientific Name", "Common Name", "SARA Status", "COSEWIC Status", all_of(extraCols))

  allSpeciesData$`Scientific Name` <- italicize_col(allSpeciesData$`Scientific Name`)
  sarData$`Scientific Name` <- italicize_col(sarData$`Scientific Name`)

  row.names(allSpeciesData) <- NULL
  row.names(sarData) <- NULL

  outList <- list("allSpecies" = allSpeciesData, "sarData" = sarData)
  return(outList)
}


##### - create_table_MARFIS ##################################
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
create_table_MARFIS <- function(data_sf, speciesTable, sarTable) {
  #absentCode <- "&nbsp;-&nbsp;"
  #presentCode <- "&#x2714;"
  
  absentCode <- "\U2013"
  presentCode <- "\U2714"
  
  nafoList <- as.list(unique(data_sf$NAFO))
  # nafoList <- append(nafoList, "4WG")
  nafoSpeciesFilter <- dplyr::filter(speciesTable, NAFO %in% nafoList)
  # sometime for loops are just better and R apply's can go hide in a hole:
  for (nafoStr in nafoList) {
    nafoSpeciesFilter[nafoStr] = nafoSpeciesFilter$NAFO == nafoStr
  }
  
  finalDf <- nafoSpeciesFilter %>% 
    dplyr::group_by(SPECIES_NAME) %>% 
    dplyr::summarise_all(max)%>%
    base::subset(select = -c(NAFO))
  
  cleanDf <- dplyr::rename(finalDf, Common = SPECIES_NAME) %>%
    dplyr::mutate(Common = stringr::str_to_sentence(Common))
  
  for (nafoStr in nafoList) {
    cleanDf[nafoStr] = ifelse(dplyr::pull(cleanDf[nafoStr]), presentCode, absentCode)
  }  
  
  
  sarData <- dplyr::filter(sarTable, Common_Name_MARFIS %in% nafoSpeciesFilter$SPECIES_NAME)
  
  if (nrow(sarData>0)){
    sarData <- dplyr::select(sarData, 'Scientific Name', 'Common Name',
                             "SARA status","COSEWIC status")
    sarData$`Scientific Name` <- italicize_col(sarData$`Scientific Name`)
    row.names(sarData) <- NULL    
  } else {
    sarData <- NULL
  }
  
  outList <- list("allSpecies" = cleanDf, "sarData" = sarData)
  return(outList)
}


##### - create_table_ISDB ##################################
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

create_table_ISDB <- function(data_sf, speciesTable, sarTable) {
  #absentCode <- "&nbsp;-&nbsp;"
  #presentCode <- "&#x2714;"
  
  absentCode <- "\U2013"
  presentCode <- "\U2714"
  
  nafoList <- as.list(unique(data_sf$NAFO))
  # nafoList <- append(nafoList, "4WG")
  nafoSpeciesFilter <- dplyr::filter(speciesTable, NAFO %in% nafoList)
  # sometime for loops are just better and R apply's can go hide in a hole:
  for (nafoStr in nafoList) {
    nafoSpeciesFilter[nafoStr] <- nafoSpeciesFilter$NAFO == nafoStr
  }
  
  finalDf <- nafoSpeciesFilter %>% 
    dplyr::group_by(SCIENTIFIC) %>% 
    dplyr::summarise_all(max)%>%
    base::subset(select = -c(NAFO))
  
  cleanDf <- dplyr::rename(finalDf, Scientific = SCIENTIFIC) %>%
    dplyr::rename(Common = COMMON) %>%
    dplyr::mutate(Scientific = stringr::str_to_sentence(Scientific)) %>%
    dplyr::mutate(Common = stringr::str_to_sentence(Common))
  
  cleanDf$Scientific <- italicize_col(cleanDf$Scientific)
  
  for (nafoStr in nafoList) {
    cleanDf[nafoStr] = ifelse(dplyr::pull(cleanDf[nafoStr]), presentCode, absentCode)
  }  
  
  
  sarData <- dplyr::filter(sarTable, SCIENTIFICNAME %in% nafoSpeciesFilter$SCIENTIFIC)
  
  if (nrow(sarData>0)){
    sarData <- dplyr::select(sarData, 'Scientific Name', 'Common Name',
                             "SARA status","COSEWIC status")
    sarData$`Scientific Name` <- italicize_col(sarData$`Scientific Name`)
    row.names(sarData) <- NULL    
  } else {
    sarData <- NULL
  }

  outList <- list("allSpecies" = cleanDf, "sarData" = sarData)
  return(outList)
}
  

##### - sfcoords_as_cols  ##################################
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
table_crit <- function(CCH_sf, lang) {

  if (lang == "EN"){
    critTableCols <- c("Common_Name_EN", "Population_EN", "Waterbody", "SARA_Status", 
                       "COSEWIC_Status", "Area_Status")
    critTableNames <- c("Common Name", "Population", "Area", "SARA status",
                        "COSEWIC Status", "Area Status")
  } else if (lang =="FR") {
    critTableCols <- c("Common_Name_FR", "Population_FR", "Waterbody", "SARA_Status")
    critTableNames <- c("Nom Commun", "Population", "Region", "Statut LEP")
  } else {
    stop("Specify Critical Habitat Table language choice (EN/FR)")
  }

  if (!is.null(CCH_sf)){
    critTable <- dplyr::select(CCH_sf, all_of(critTableCols))
    critTable <- sf::st_drop_geometry(critTable)
    names(critTable) <- critTableNames
  } else {
    # only set names after init to preserve spaces etc.
    critTable <- data.frame("a"=NA, "b"=NA, "c"=NA, "d"=NA)
    names(critTable) <- critTableNames
  }

  # remove rows with NA area, dump duplicates:
  critTable <- distinct(critTable[!is.na(critTable$Area), ])

  if (!nrow(critTable) >= 1){
    return(NULL)
  } else {
    return(critTable)
  }
}

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
    EBSATable <- sf::st_drop_geometry(dplyr::select(EBSA_sf, c(Report, Report_URL,
                                                           Name, Bioregion)))
    EBSATable <- unique(EBSATable)
    row.names(EBSATable) <- NULL
    names(EBSATable) <- c("Report", "Report URL", "Location", "Bioregion")
  } else if (lang=="FR" & !is.null(EBSA_sf)) {
    EBSATable <- sf::st_drop_geometry(dplyr::select(EBSA_sf, c(Rapport, RapportURL,
                                                           Nom, Bioregion)))
    EBSATable <- unique(EBSATable)
    names(EBSATable) <- c("Report", "Report URL", "Location", "Bioregion")
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
    names(mpaTable) <- c("Site #", "Site Name", "Status", "Size (km2)")
  } else if (lang=="FR" & !is.null(mpa_sf)) {
    names(mpaTable) <- c("Site #", "Site Name", "Status", "Size (km2)")
  } else {
    names(mpaTable) <- c("Site #", "Site Name", "Status", "Size (km2)")
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
    data_sf <- sf::st_drop_geometry(data_sf)
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
  #absentCode <- "&nbsp;-&nbsp;"
  #presentCode <- "&#x2714;"

  absentCode <- "\U2013"
  presentCode <- "\U2714"
  
  if (!is.null(dataTable)){
    if (indexCol == attributeCol) {
      dataTable <- distinct(dataTable, !!sym(indexCol))
    }
    dataTable$summaryCol <-ifelse(dataTable[[attributeCol]] > 0, presentCode, absentCode)
    dataTable$speciesCol <- dataTable[[indexCol]]
    dataTable <- dplyr::filter(dataTable, speciesCol %in% sarSummary$Species)
  } else {
    # dataTable was null:
    dataTable <- sarSummary
    dataTable$speciesCol <- dataTable$Species
    dataTable$summaryCol <- rep(absentCode, nrow(sarSummary))
  }

  sarSummary[[dbName]] <- dplyr::left_join(sarSummary, dataTable, by=c("Species" = "speciesCol"))$summaryCol
  
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
    dataTable <- dplyr::filter(dataTable, speciesCol %in% summaryTable$Species)
    dataTable <- dplyr::filter(dataTable, lengths(summaryCol) > 0)
    tempCol <- dplyr::left_join(summaryTable, dataTable, by=c("Species"="speciesCol"))$summaryCol

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

isle_madame_table <- function(data_sf, cols, colnames){
  if (is.null(data_sf)) {
    return(NULL)
  }
  if ("JAN" %in% names(data_sf)) {
    currentMonth <- toupper(format(Sys.Date(), "%b"))
    data_sf[[currentMonth]][sf::st_drop_geometry(data_sf)[currentMonth] == "see OVERALL_PRESENCE"] <- ""
    monthName <- month.name[as.integer(format(Sys.Date(), "%m"))]
    cols <- append(cols, currentMonth)
    colnames <- append(colnames, paste(monthName, "presence"))
  }  
  data_df <- sf::st_drop_geometry(data_sf)
  data_sf$geometry <- NULL
  data_df <- dplyr::select(data_df, cols) %>%
    unique() 
  names(data_df) <- colnames
  rownames(data_df) <- NULL
  return(data_df)
}

add_row_to_intro_summary <- function(introSummary, name, result) {
  #absentCode <- "&nbsp;-&nbsp;"
  #presentCode <- "&#x2714;"
  
  absentCode <- "\U2013"
  presentCode <- "\U2714"
  
  rowNum <- nrow(introSummary) + 1
  
  introSummary <- rbind(introSummary, 
                        data.frame(No=rowNum,
                                   Datasource=name,
                                   Results=ifelse(result, presentCode,
                                                  absentCode )))
  return(introSummary)
  
}

# ---------OECM_report-------
# Generates table for Other Effective Area Based Conservation Measures data
# Inputs:
# OECM_sf: OECM data clipped to area of interest
# lang: "EN" or "FR", toggles language of data returned
#
# Outputs:
# Directly writes table
#
OECM_report <- function(OECM_sf, lang="EN") {
  OECMTable <- NULL
  if (lang=="EN" & !is.null(OECM_sf)) {
    OECMTable <- sf::st_drop_geometry(dplyr::select(OECM_sf, c(NAME_E, OBJECTIVE,
                                                               PROHIBITIONS,REGION_E, URL_E)))
    OECMTable <- unique(OECMTable)
    row.names(OECMTable) <- NULL
    names(OECMTable) <- c("Name", "Objective", "Prohibitions","Region", "Report url")
  } else if (lang=="FR" & !is.null(OECM_sf)) {
    OECMTable <- sf::st_drop_geometry(dplyr::select(OECM_sf, c(NAME_F, OBJECTIF, INTERDICTIONS,
                                                               REGION_F, URL_F)))
    OECMTable <- unique(OECMTable)
    names(OECMTable) <- c("Nom", "Objectif", "Interdictions", "Region", "Rapport url")
    row.names(OECMTable) <- NULL
    
  }
  return(OECMTable)
}

# ---------DCSB_report-------
# Generates table for Coral and Sponge Significant Benthic Areas in Eastern Canada data
# Inputs:
# DCSB_sf: DCSB data clipped to area of interest
# lang: "EN" or "FR", toggles language of data returned
#
# Outputs:
# Directly writes table
#
DCSB_report <- function(DCSB_sf, lang="EN") {
  DCSBTable <- NULL
  if (lang=="EN" & !is.null(DCSB_sf)) {
    DCSBTable <- sf::st_drop_geometry(dplyr::select(DCSB_sf, c(BioGeo_Reg, significantBenthicAreas,
                                                               Map_Ref)))
    DCSBTable <- unique(DCSBTable)
    row.names(DCSBTable) <- NULL
    names(DCSBTable) <- c("Biogeographic Region", "Significant Benthic Areas", "Map Reference Number from Publication")
  } else if (lang=="FR" & !is.null(DCSB_sf)) {
    DCSBTable <- sf::st_drop_geometry(dplyr::select(DCSB_sf, c(BioGeo_Reg, significantBenthicAreas,
                                                               Map_Ref)))
    DCSBTable <- unique(DCSBTable)
    names(DCSBTable) <- c("Région Biogéographique", "Zones Benthiques Importantes", "Carte Numéro de Référence de la Publication")
    row.names(DCSBTable) <- NULL
    
  }
  return(DCSBTable)
}

# ---------SUBSTRATE_report-------
# Generates table for A substrate classification for the Inshore Scotian Shelf and Bay of Fundy, Maritimes Region data
# Inputs:
# SUBSTRATE_sf: Substrate data clipped to area of interest
# lang: "EN" or "FR", toggles language of data returned
#
# Outputs:
# Directly writes table
#
SUBSTRATE_report <- function(SUBSTRATE_sf, lang="EN") {
  SUBSTRATETable <- NULL
  if (lang=="EN" & !is.null(SUBSTRATE_sf)) {
    SUBSTRATETable <- sf::st_drop_geometry(dplyr::select(SUBSTRATE_sf, c(All_Bottom_Type, Substrate)))
    SUBSTRATETable <- unique(SUBSTRATETable)
    row.names(SUBSTRATETable) <- NULL
    names(SUBSTRATETable) <- c("Bottom Type", "Substrate")
  } else if (lang=="FR" & !is.null(SUBSTRATE_sf)) {
    SUBSTRATETable <- sf::st_drop_geometry(dplyr::select(SUBSTRATE_sf, c(All_Bottom_Type, Substrate)))
    SUBSTRATETable <- unique(SUBSTRATETable)
    names(SUBSTRATETable) <- c("Type de fond", "Substrat")
    row.names(SUBSTRATETable) <- NULL
    
  }
  return(SUBSTRATETable)
}
