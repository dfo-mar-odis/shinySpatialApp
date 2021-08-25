# script to walkthrough hex_data handling:



marfis_setup <- function() {
  isdb2015_demo <- readRDS(here::here("hex_data/isdb2015_DemoHex_sf.RDS"))
  marfis2015_demo <- readRDS(here::here("hex_data/marf2015_DemoHex_sf.RDS"))
  isdb_codes <- readRDS(here::here("hex_data/SPECIES_ISDB.RDS"))
  
  marfisClipped <- master_intersect(marfis2015_demo, region, studyArea, bboxMap)
  isdbClipped <- master_intersect(isdb2015_demo, region, studyArea, bboxMap)
  
  
  specList <- filter(MARFISSPECIESCODES, COMMONNAME %in% listed_species$Common_Name_MARFIS)$SPECIES_CODE

}



# helper function, replaces a column name with it's common name from the 
# corresponding marifs or isdb species table. 
# returns a list containing the common name and the updated data_sf object. 
get_spec_names <- function(colNames, marfis=FALSE) {
  if (marfis) {
    attrNums <- col_to_marfis(colNames)
    lookupTable <- MARFISSPECIESCODES
    lookupCol <- "SPECIES_CODE"
    cnameCol <- "COMMONNAME"
  }
  else {
    attrNums <- col_to_isdb(colNames)
    lookupTable <- ISSPECIESCODES
    lookupCol <- "SPECCD_ID"
    cnameCol <- "Common Name"
    
  }
  # get species name from table
  specNames <- filter(lookupTable, get(lookupCol) %in% attrNums)[[cnameCol]]
  return(specNames)
}


# adds marfis or isdb data for a given species to an input ggplot. 
plot_isdb_marfis <- function(colName, ggplotIn, data_sf, maxCnt) {
  
  if (colName %in% names(data_sf)) {
    # drop zero rows:
    plot_sf <- dplyr::filter(data_sf, (get(colName) > 0))
    if (nrow(plot_sf) > 0) {
      outMap <- ggplotIn +
        geom_sf(data=plot_sf, aes(fill=get(colName)), color = NA) + 
        scale_fill_viridis_c(option = "plasma", name="", limits=c(0, maxCnt)) + 
        theme(legend.position = "none") +
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank())
      
      return(outMap)
    } # end of data results in area check
    else {
      return(NULL)
    }
  } # end of column name in data check
  else {
    return(NULL)
  }
}


marfis_to_col <- function(marfisNum){
  fullColNames <- paste("X", marfisNum, "_SU", sep = "")
  return(fullColNames)
}

isdb_to_col <- function(isdbNum){
  fullColNames <- paste("EST_COMBINED_WT_", isdbNum, sep = "")
  return(fullColNames)
}

col_to_marfis <- function(marfisCol){
  marfisNum <- gsub("_SU", "", gsub("X", "", marfisCol))
  return(marfisNum)
}

col_to_isdb <- function(isdbCol){
  isdbNum <- gsub("EST_COMBINED_WT_", "", isdbCol)
  return(isdbNum)
}

  


clean_isdb_marfis_sf <- function(data_sf, specNumList, marfis=FALSE){
  
  # convert numeric list to list of col names:
  if (marfis) {
    fullColNames <- marfis_to_col(specNumList)
  }
  else {
    fullColNames <- isdb_to_col(specNumList)
  }
  
  # only select columns asked for that have data:
  data_sf <- dplyr::select(data_sf, any_of(fullColNames))
  noGeom <- sf::st_drop_geometry(data_sf)
  noGeom <- dplyr::select(noGeom, which(colSums(noGeom) > 0))
  scaleLim <- max(noGeom)
  colNamesUsed <- names(noGeom)
  data_sf <- dplyr::select(data_sf, all_of(colNamesUsed))
  
  specNames <-get_spec_names(colNamesUsed, marfis)
  names(data_sf) <- append(specNames, "geometry")
  
  
  
  outList <- list(data_sf = data_sf,
                  specList = specNames,
                  scaleLim = scaleLim)
  return(outList)
}
  

#
# INPUTS:
# baseGgplot: background ggplot object for all plots with land, axis, etc.  regionMap/areaMap
# data_sf: data to plot
# speciesCodeList: List of species codes indicating which species to plot
# marfis: boolean indicating whether the data is from marfis or isdb
#
#
plot_marfis_grid<-function(baseGgplot, data_sf, speciesCodeList, marfis, ncol=3) {
  
  cleanData <- clean_isdb_marfis_sf(data_sf, speciesCodeList, marfis)
  
  plotList <- lapply(cleanData$specList, plot_isdb_marfis, ggplotIn=baseGgplot, 
                     data_sf=cleanData$data_sf, maxCnt=cleanData$scaleLim)
  plotList <- plotList[!sapply(plotList, is.null)]
  
  plotList <- plotList[1:9]
  plotLabels <- cleanData$specList[1:9]
  
  legend <- cowplot::get_legend(plotList[[1]] + theme(legend.position = "bottom"))
  p_grid <- cowplot::plot_grid(plotlist = plotList, labels = plotLabels, label_size = 8)
  cowplot::plot_grid(p_grid,
                     legend, 
                     ncol = 1, 
                     rel_heights = c(2, 0.2),
                     scale=0.9) +
    draw_label(expression(paste("Longitude ",degree,"W",sep="")), x=0.5, y= 0, vjust=-4, angle= 0) +
    draw_label(expression(paste("Latitude ",degree,"N",sep="")), x=0, y=0.5, vjust= 1.5, hjust=-1, angle=90)
  
}




