# #Rasters - Grid of 4 cetacean priority habitat

plot_cetaceans_4grid<-function(fin_whale_sf, harbour_porpoise_sf,
                               humpback_whale_sf, sei_whale_sf, studyArea,
                               landLayer, bufKm, bounds_sf) {
  # buf is in km, and now converted to degrees
  buf <- bufKm / 100
  bufLong <- buf * 2

  # bounding box
  bbox <- st_bbox(studyArea)
  
  bboxBuf <- bbox
  
  bboxBuf["xmin"] <- (bbox$xmin) - bufLong
  bboxBuf["xmax"] <- (bbox$xmax) + bufLong
  bboxBuf["ymin"] <- (bbox$ymin) - buf
  bboxBuf["ymax"] <- (bbox$ymax) + buf
  
  land <- sf::st_crop(landLayer, bboxBuf)
  bound <- sf::st_crop(bounds_sf, bboxBuf)
  
  #Fin Whale
  finWhalePlot <- whale_ggplot(fin_whale_sf, bound, land, studyArea,
                               "Fin Whale", bboxBuf)
 
  #Harbour Porpoise
  harbourPorpoisePlot <- whale_ggplot(harbour_porpoise_sf, bound, land, studyArea,
                                    "Harbour Porpoise", bboxBuf)

  #humpback whale
  humpbackWhalePlot <- whale_ggplot(humpback_whale_sf, bound, land, studyArea,
                                    "Humpback Whale", bboxBuf)
  
  #Sei Whale
  seiWhalePlot <- whale_ggplot(sei_whale_sf, bound, land, studyArea,
                               "Sei Whale", bboxBuf)

  #Arrange all 4 cetaceans into grid
  gridExtra::grid.arrange(finWhalePlot, harbourPorpoisePlot, humpbackWhalePlot,
                          seiWhalePlot,
                          bottom = expression(paste("Longitude ",degree,"N",sep="")),
                          left = expression(paste("Latitude ",degree,"N",sep="")),
                          nrow = 2)
}


# Rockweed stats
rockweedStats<- function(rockweed_sf, studyArea) {

  # clip rockweed to study area
  rw = sf::st_crop(sf::st_make_valid(rockweed_sf), studyArea)
  rw$area = sf::st_area(rw) # add column with areas of the polygons

  # make a table, sum the areas for different presences
  noRecords = as.data.frame(table(rw$RWP))
  noRecords$Var1 = as.numeric(noRecords$Var1)
  totalArea = aggregate(as.numeric(rw$area), list(rw$RWP), sum)
  stats = merge(noRecords, totalArea, by.x="Var1", by.y="Group.1")
  stats = rbind(stats, colSums(stats))
  names(stats) = c("RWP", "noPolygons", "Area_m2")
  stats$Category = ""
  stats$Category[stats$RWP==1] = "Rockweed present"
  stats$Category[stats$RWP==2] = "Rockweed likely present"
  stats$Category[stats$RWP==5] = "Unknown vegetation"
  stats$Category[stats$RWP==0] = "Rockweed not present"
  stats$Category[nrow(stats)] = "Total intertidal vegetation"

  stats$Area_km2 = round(stats$Area_m2 / 1000) / 1000
  stats = stats[, c("Category", "noPolygons", "Area_km2")]

  return(stats)

}


whale_ggplot <- function(whale_sf, bound, landLayer, studyArea, plotTitle, plotBbox) {

  rawPlot <- ggplot() +
    geom_sf(data=whale_sf, fill="#F3E73B", col="#F3E73B") +
    geom_sf(data=bound, col="darkgrey", linetype="dashed", size=1.1) +
    geom_sf(data=landLayer, fill=c("grey90"), col="black") +
    geom_sf(data=studyArea, fill=NA, col="red", size=1) + 
    ggtitle(plotTitle) 
  
  outPlot <- format_ggplot(rawPlot, plotBbox)
  
  return(outPlot)
}




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

# plots marfis and isdb data by species in a grid
plot_marfis_grid<-function(baseGgplot, data_sf, speciesCodeList, marfis=FALSE) {
  
  plotList <- lapply(speciesCodeList, plot_isdb_marfis, ggplotIn=baseGgplot, data_sf=data_sf, marfis=marfis)
  plotList <- plotList[!sapply(plotList, is.null)]
  #Arrange all plots in a grid
  gridExtra::grid.arrange(grobs=plotList,
                          bottom = expression(paste("Longitude ",degree,"N",sep="")),
                          left = expression(paste("Latitude ",degree,"N",sep="")),
                          ncol=2)
}





