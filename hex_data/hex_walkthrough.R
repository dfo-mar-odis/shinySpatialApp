# script to walkthrough hex_data handling:




isdb2015_demo <- readRDS(here::here("hex_data/isdb2015_DemoHex_sf.RDS"))
marfis2015_demo <- readRDS(here::here("hex_data/marf2015_DemoHex_sf.RDS"))
isdb_codes <- readRDS(here::here("hex_data/SPECIES_ISDB.RDS"))

# helper function, replaces a column name with it's common name from the 
# corresponding marifs or isdb species table. 
# returns a list containing the common name and the updated data_sf object. 
set_spec_col_name <- function(data_sf, fullColName, attrNum, marfis=FALSE) {
  if (marfis) {
    lookupTable <- MARFISSPECIESCODES
    lookupCol <- "SPECIES_CODE"
    cnameCol <- "COMMONNAME"
  }
  else {
    lookupTable <- ISSPECIESCODES
    lookupCol <- "SPECCD_ID"
    cnameCol <- "Common Name"
    
  }
  # get species name from table
  specName <- filter(lookupTable, get(lookupCol)==attrNum)[[cnameCol]]
  data_sf[[specName]] <- data_sf[[fullColName]]
  outList <- list(specName = specName, data_sf=data_sf)
  return(outList)
}


# adds marfis or isdb data for a given species to an input ggplot. 
plot_isdb_marfis <- function(specNum, ggplotIn, data_sf, marfis=FALSE) {
  if (marfis) {
    fullColName <- glue("X", specNum, "_SU")
  }
  else {
    fullColName <- glue("EST_NUM_CAUGHT_", specNum)
  }
  
  if (fullColName %in% names(data_sf)) {
    poly_sf <- filter(data_sf, get(fullColName) > 0)
    specData <- set_spec_col_name(poly_sf, fullColName, specNum, marfis)
    if (nrow(specData$data_sf) > 0) {
      outMap <- ggplotIn +
        geom_sf(data=specData$data_sf, aes(fill=get(specData$specName)), color = NA) + 
        scale_fill_viridis_c(option = "plasma",  name=str_wrap(toString(specData$specName), 8))
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

marfisClipped <- master_intersect(marfis2015_demo, region, studyArea, bboxMap)
isdbClipped <- master_intersect(isdb2015_demo, region, studyArea, bboxMap)


plot_marfis_grid<-function(baseGgplot, data_sf, speciesCodeList, marfis=FALSE) {
  
  plotList <- lapply(speciesCodeList, plot_isdb_marfis, ggplotIn=baseGgplot, data_sf=data_sf, marfis=marfis)
  plotList <- plotList[!sapply(plotList, is.null)]
  #Arrange all plots in a grid
  gridExtra::grid.arrange(grobs=plotList,
                          bottom = expression(paste("Longitude ",degree,"N",sep="")),
                          left = expression(paste("Latitude ",degree,"N",sep="")),
                          ncol=3)
}




specList <- filter(MARFISSPECIESCODES, COMMONNAME %in% listed_species$Common_Name_MARFIS)$SPECIES_CODE






