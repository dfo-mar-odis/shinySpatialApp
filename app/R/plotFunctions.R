
# Function for plotting sf data for the reproducible report.
#
# Inputs:
# 1. baseMap = map object, either areaMap or regionMap
# 2. data_sf: sf data to be plotted, can contain either point or polygon data
#    (ideally, pre-clipped to map area with the master_intersect function, using bboxMap, or regionBox)
# 3. attribute: column name of factor data in data_sf. 
#               this attribute name will appear in the legend. For single color polygons leave blank
# 4. legendName: string, sets the name of the legend for cases where the attribute is not appropriate. Defaults to
#                the attribute.  
# 5. legendColours: list of colours used to set the scale.  eg. WhaleCol. Only used for point data
#    
# 
# Created by Quentin Stoyel, September 2, 2021 for reproducible reporting project

plot_rr_sf <- function(baseMap, data_sf, attribute=NULL, ...) {
  if (inherits(sf::st_geometry(data_sf), "sfc_POINT")) {
    
    outPlot <- plot_points(baseMap, data_sf, attribute=attribute, ...)
    
  } else if (inherits(sf::st_geometry(data_sf), c("sfc_POLYGON", "sfc_MULTIPOLYGON", "sfc_GEOMETRY"))) {
    
    outPlot <- plot_polygons(baseMap, data_sf, attribute=attribute, ...)
    
  }
  
  else {
    stop("Geometry type not supported")
  }
  return(outPlot)
}



# helper function, extracts the scale bar from either the areaMap or regionMap
get_scale_bar_layer <- function(inPlot) {
  scaleBarLayer <- lapply(inPlot$layers, function(inLayer) if("GeomScaleBar" %in% class(inLayer$geom)) inLayer else NULL)
  scaleBarLayer <- scaleBarLayer[!sapply(scaleBarLayer, is.null)]
  return(scaleBarLayer)
}

# helper function, extracts the watermark layer from either the areaMap or regionMap
get_watermark_layer <- function(inPlot) {
  watermarkLayer <- lapply(inPlot$layers, function(inLayer) if("GeomCustomAnn" %in% class(inLayer$geom)) inLayer else NULL)
  watermarkLayer <- watermarkLayer[!sapply(watermarkLayer, is.null)]
  return(watermarkLayer)
}


# helper function, extracts the study_box_layer from either the areaMap or regionMap
# selection criteria is based off of colour, use with care.
get_study_box_layer <- function(inPlot) {
  studyBoxLayer <- lapply(inPlot$layers, function(inLayer) if("red" %in% c(inLayer$aes_params$colour)) inLayer else NULL)
  studyBoxLayer <- studyBoxLayer[!sapply(studyBoxLayer, is.null)]
  return(studyBoxLayer)
}



# Function for plotting point data for the reproducible report.
#
# Inputs:
# 1. baseMap = map object, either areaMap or regionMap
# 2. data_sf: sf data to be plotted 
#    (ideally, pre-clipped to map area with the master_intersect function, using bboxMap, or regionBox)
# 3. attribute: column name of factor data in data_sf. 
#               this attribute name will appear in the legend. For single color polygons leave blank
# 4. legendName: string, sets the name of the legend for cases where the attribute is not appropriate. Defaults to
#                the attribute.  
# 5. legendColours: list of colours used to set the scale.  eg. WhaleCol
#    
# 
# Created by Quentin Stoyel, September 2, 2021 for reproducible reporting project

plot_points <- function(baseMap, data_sf, attribute=NULL, legendName="", colorMap=NULL, diffShapes=FALSE) {
  
  # extract scaleBar layer to ensure it plots over polygons/study area box
  scaleBarLayer = get_scale_bar_layer(baseMap)
  studyBoxLayer = get_study_box_layer(baseMap)
  watermarkLayer = get_watermark_layer(baseMap)
  
  # axis limits based on baseMap
  axLim = ggplot2::coord_sf(xlim=baseMap$coordinates$limits$x, 
                            ylim=baseMap$coordinates$limits$y, expand=FALSE) 
  shapeLayer <- NULL
  
  if (is.null(attribute)) {
    # just plot raw data (no colors)
    dataLayer <- geom_sf(data = data_sf, size = 2, shape = 20) 
    legendLayer <- NULL
  } else {
    dataLayer <- geom_sf(data = data_sf, aes(color=!!sym(attribute)), size = 2.5, shape = 20)  
    
    if (is.null(colorMap)){
      colorMap <- get_rr_color_map(data_sf[[attribute]])
    } else {
      colorMap <- colorMap[names(colorMap) %in% data_sf[[attribute]]]
      if (diffShapes){
        shapeLabels <- names(colorMap)
        shapeValues <- rep_len(15:20, length(shapeLabels))
        dataLayer <- geom_sf(data = data_sf, aes(color=!!sym(attribute), shape=!!sym(attribute)), size = 2.5)
        shapeLayer <- scale_shape_manual(labels = shapeLabels, values = shapeValues, name=legendName)  
      }
    }
    legendLayer <- scale_colour_manual(values=colorMap, name=legendName)  
    

    
    
  }
    
  pointMap <- baseMap +
    dataLayer +
    shapeLayer +
    legendLayer +
    axLim +
    watermarkLayer +
    studyBoxLayer +
    scaleBarLayer
  
  return(pointMap) 
}


# Function for plotting polygons for the reproducible report.
#
# Inputs:
# 1. baseMap = map object, either areaMap or regionMap
# 2. polyData: polygon data to be plotted 
#    (pre-clipped to map area with the master_intersect function, using bboxMap, or regionBox)
# 3. attribute: attribute in the polygon data to be plotted (column name as a string, e.g. "Activity", or "Rockweed"; 
#               this attribute name will appear in the legend. For single color polygons use attribute="NONE".
# 4. legendName: string, sets the name of the legend for cases where the attribute is not appropriate. Defaults to
#                the attribute.  
#    
# 
# Examples of use:
#
#   To plot on region map: plot_polygons(regionMap, regionBox, studyBox_geom, data$regionPoly, attribute="NONE")
#                          (only parameters to change are data$regionPoly and attribute)
#
#   To plot area map: plot_polygons(areaMap, bboxMap, studyBox_geom, data$regionPoly, attribute="NONE")
#                          (only parameters to change are data$regionPoly and attribute)
#
# Created by Gordana Lazin, July 2, 2021 for reproducible reporting project


plot_polygons <- function(baseMap, polyData, attribute, legendName=attribute,
                          outlines=TRUE, colorMap=NULL, getColorMap=FALSE) {
  
  scaleBarLayer = get_scale_bar_layer(baseMap)
  studyBoxLayer = get_study_box_layer(baseMap)
  watermarkLayer = get_watermark_layer(baseMap)
  
  clr = "black" #color for outlining polygons
  
  # axis limits to the plot
  axLim = ggplot2::coord_sf(xlim=baseMap$coordinates$limits$x, 
                            ylim=baseMap$coordinates$limits$y, expand=FALSE) 
  
  
  # there are two types of plots: 
  # Case 1: all polygons are one color, no legend,
  # case 2: polygons are colored based on the "attribute" column, legend is included
  
  if (toupper(attribute) == "NONE") { # Case 1: plotting all polygons in one color
    
    polyPlot <- geom_sf(data=polyData, fill="#56B4E9", col=clr)
    polyFill <- NULL
    polyOutline <- NULL
    
    
  } else { # Case 2: plotting polygons in different colors based on "attribute" column in the data
    
    if (is.null(colorMap)){
      colorMap <- get_rr_color_map(polyData[[attribute]])
    } else {
      colorMap <- colorMap[names(colorMap) %in% polyData[[attribute]]]
    }

    polyFill <- scale_fill_manual(values=colorMap, name=legendName)
    
    if (outlines) {
      polyOutline <- NULL
      polyPlot <- geom_sf(data=polyData, aes(fill=!!sym(attribute)), colour=clr)
      
    }
    else {
      polyPlot <- geom_sf(data=polyData, aes(fill=!!sym(attribute), col=!!sym(attribute)))
      polyOutline <- scale_color_manual(values=colorMap, guide="none")  
    }
    
    
  }
 
  # this is how general plotting function should look like
    
    polyMap <- baseMap +
      polyPlot +
      polyFill +
      polyOutline +
      axLim +
      watermarkLayer +
      studyBoxLayer +
      scaleBarLayer
    
    if (getColorMap) {
      outList <- list(colorMap=colorMap,
                      polyMap=polyMap)
      return(outList)
    } else {
      return(polyMap)  
    }
    
  
}


get_rr_color_map <- function(dataCol) {
  colorNames <- unique(dataCol)
  colorNames <- colorNames[order(colorNames)]
  numColors <- length(colorNames)
  rrColorPalette <- c("#009E73", "#E69F00", "#0072B2", "#CC79A7", "#F0E442", 
                      "#D55E00", "#56B4E9","#999999")
  if (numColors > 0) {
    if(numColors > length(rrColorPalette)){
      colorMap <- hcl.colors(length(colorNames))
    } else {
      colorMap <- rrColorPalette[1:numColors]
    }
    names(colorMap) <- colorNames
  }
  return(colorMap)
}


# --------plot cetaceans 4 grid------------
# This function produces a 2x2 grid of the four cetacean plots.
#
# Inputs:
# 1. fin_whale_sf: sf object for fin whales.
# 2. harbour_porpoise_sf: sf object for harbour porpoises.
# 3. humpback_whale_sf: sf object for humpback whales.
# 4. sei_whale_sf: sf object for sei whales.
# 5. studyArea: polygon of the study area (sf object, defined by the user in the shiny app)
# 6. landLayer: land borders for the plots, could be 10K or 50K scale.
# 7. bufKm: buffer, in km to be placed around the plots.
# 8. bounds_sf: Canada border, including water.
#
# Outputs: ggplot object with the 4 plots.

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

# --------Maps Setup------------
# This function produces a list of the necessary data used to generate plots in the report
#
# Inputs:
# 1. studyArea: polygon of the study area (sf object, defined by the user in the shiny app)
# 2. site: polygon of a study site (like aquaculture site);for now it is a centroid (a point, centre of studyArea)
#    in the past we plotted the site polygon and site is kept for now as a placeholder for the future
# 3. region: Polygon describing the region of the report
# 4. areaLandLayer: land borders for the areaMap, could be 10K or 50K scale (preloaded)
# 5. regionLandLayer: land borders for the regionMap, could be 10K or 50K scale (preloaded)
# 6. CANborder: Canada border, including water (preloaded: bounds_sf)
#
# Outputs: list containing 8 items
# 1. studyBox_geom, bounding box of the study area
# 2. studyArea, input studyArea
# 3. site, input site
# 4. region, input region
# 5. areaMap, map of the study area, used as a base map for plots in the report
# 6. bboxMap, bounding box of the areaMap
# 7. regionBox, bounding box of the region
# 8. regionMap, map of the study region, used as a base map for plots in the report
#
# Written by Quentin Stoyel for reproducible reporting project, September 2, 2021

maps_setup <- function(studyArea, site, region, areaLandLayer, regionLandLayer, CANborder){
  # The following defines studyBox geometry "look". studyBox_geom is input into area map or can be added to any map later
  studyBox_geom <- geom_sf(data=studyArea, fill=NA, col="red", size=1)
  
  # The following plots area map using function (output is a list)
  areaMapList <- area_map(studyArea, site, areaLandLayer, 5, CANborder, studyBox_geom)
  
  # The following separates items in the output list: first item is a map and second is a bounding box of the map
  areaMap <- areaMapList[[1]] # map
  bboxMap <- areaMapList[[2]] #bounding box of the map
  
  # Bounding box for the region
  regionBox <- sf::st_bbox(region)
  
  # Create the regional map
  regionMap <- region_map(regionBox, studyArea, regionLandLayer, CANborder) 
  
  outlist <- list("studyBox_geom" = studyBox_geom,
                  "studyArea" = studyArea,
                  "site" = site,
                  "areaMap" = areaMap,
                  "bboxMap" = bboxMap,
                  "region" = region,
                  "regionBox" = regionBox,
                  "regionMap" = regionMap
  )
  return(outlist)
}





# ----- AREA MAP -----
# This function produces a map of the area surrounding a box of the study area using ggplot.
# The extent of the area to plot around the studyArea is defined by "bufKm" parameter in km (sets "zoom")
# This function was created for searchPEZ and it is used as a "basemap" for other plots
#
# Inputs:
# 1. studyArea: polygon of the study area (sf object, defined by the user in the shiny app)
# 2. site: polygon of a study site (like aquaculture site);for now it is a centroid (a point, centre of studyArea)
#    in the past we plotted the site polygon and site is kept for now as a placeholder for the future
# 3. landLayer: polygon developed for Atlantic canada, could be 10K or 50K scale (preloaded)
# 4. bufKm: how many km around the study area to plot (acts like "zoom")
# 5. CANborder: Canada border, including water (preloaded)
# 6. studyBoxGeom: geometry portraying study box (defines the "look" of the study box, defined in the main script)
#
# Outputs: list containing 2 items
# 1. map, that can be used as a basemap for adding data layers
# 2. bounding box of the map that can be used for cropping datasets
#
# Written by Gordana Lazin for reproducible reporting project, April 12, 2021
# ggplot map developed by Greg Puncher, winter/spring 2021


area_map <- function(studyArea, site, landLayer, bufKm, CANborder, studyBoxGeom) {
  
  # buf is in km, and now converted to degrees
  bufx <- bufKm / 100
  bufy <- 0.72 * bufKm / 100 # scaled degrees
  
  # bounding box for study area
  bbox <- sf::st_bbox(studyArea)
  
  widthBbox <- ((bbox$xmax) - (bbox$xmin)) * 0.72 # in scaled degrees
  heightBbox <- (bbox$ymax) - (bbox$ymin)  # in degrees
  
  if (heightBbox > 2 * widthBbox) {
    bufx <- bufx + (0.5 * (heightBbox - widthBbox))
  } else if (widthBbox > 2 * heightBbox) {
    bufy <- bufy + (0.5 * (widthBbox - heightBbox))
  }
  
  
  # create bounding box for buffer (plot area)
  bboxBuf <- bbox
  
  bboxBuf["xmin"] <- (bbox$xmin) - bufx
  bboxBuf["xmax"] <- (bbox$xmax) + bufx
  bboxBuf["ymin"] <- (bbox$ymin) - bufy
  bboxBuf["ymax"] <- (bbox$ymax) + bufy
  
  
  # crop land to plot area to speed up plotting
  land <- sf::st_crop(landLayer, bboxBuf)
  
  # crop US-Canad boundary to plot area to speed up plotting
  bound <- sf::st_crop(CANborder, bboxBuf)
  
  # configure the plot
  outPlot <- ggplot() + 
    geom_sf(data = site, fill = "yellow", col = "black", size = 0.6) +
    geom_sf(data = bound, col = "darkgrey", linetype = "dashed", size = 1.1) + # creates US boundary line, 200 nm limit
    geom_sf(data = land, fill = c("lightgrey"), col = "black", size = 0.3) +
    eval(studyBoxGeom) 
  
  outPlot <- format_ggplot(outPlot, bboxBuf)
  outList <- list(outPlot, bboxBuf)
  
  return(outList)
  
}

# ----- REGION MAP -----
# This function produces a map of the region using ggplot.
#
# Inputs:
# 1. regionBbox: bounding box of the region (defined in intro) 
# 2. studyArea: polygon of the study area (sf object, defined by the user in the shiny app)
# 3. landLayer: polygon developed for Atlantic canada, scale used for regional maps is 1:10m (land10m_sf)
# 4. CANborder: Canada border, including water (preloaded: bounds_sf)
# 
# Output: map, that can be used as a basemap for adding data layers
# 
# Modified by Gordana Lazin, June 29, 2021
# Written by Philip Greyson for reproducible reporting project, June 17, 2021
#   (modified from area_map)
# ggplot map developed by Greg Puncher, winter/spring 2021


region_map <- function(regionBbox, studyArea, landLayer, CANborder) {
  
  # subset land to plot area to speed up plotting
  land <- sf::st_crop(landLayer, regionBbox)
  
  # subset US-Canad boundary to plot area to speed up plotting
  bound <- sf::st_crop(CANborder, regionBbox)
  
  # configure the plot
  rawPlot <- ggplot() +
    geom_sf(data = bound, col = "darkgrey", linetype = "dashed", size = 1.1) + # creates US boundary line, 200 nm limit
    geom_sf(data = land, fill = c("lightgrey"), col = "black", size = 0.3) +
    geom_sf(data = studyArea, fill = NA, col = "red", size = 1)
  
  outPlot <- format_ggplot(rawPlot, regionBbox)
  
  return(outPlot)
}


#-----------Format Ggplot ----------
# Function that takes a ggplot object as input and adds preset formatting, 
# and axis labels of latitude and longitude, allows all plots
# to have a consistent style.  

format_ggplot <- function(ggplotIn, bbox) {
  
  # convert to degrees, 0.7 is bad lat/long correction factor.  
  rotTheta <- (360 /(2* pi)) * atan((bbox$ymax[[1]] - bbox$ymin[[1]]) / 
                                      (0.7 * (bbox$xmax[[1]] - bbox$xmin[[1]])))
  
  ggplotOut <- ggplotIn +
    annotation_custom(grid::textGrob("DFO Internal Use Only", rot=rotTheta,
                                     gp=grid::gpar(fontsize=30, alpha=0.5, col="grey70", fontface="bold")),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    annotation_scale(location = "bl") +
    theme_bw()+
    labs(x = expression(paste("Longitude ", degree, "W", sep = "")),
         y = expression(paste("Latitude ", degree, "N", sep = "")),
         col = "")  +
    theme(axis.title.y = element_text(size = 13))+
    theme(axis.title.x = element_text(size = 13))
  
  # crop to bbox if used:
  if (class(bbox) == "bbox") {
    ggplotOut <- ggplotOut + coord_sf(xlim = c(bbox[["xmin"]], bbox[["xmax"]]),
                                      ylim = c(bbox[["ymin"]], bbox["ymax"]), 
                                      expand = FALSE)
    
  }
  
  return(ggplotOut)
}


whale_ggplot <- function(whale_sf, bound, landLayer, studyArea, plotTitle, plotBbox) {
  
  rawPlot <- ggplot() +
    geom_sf(data=whale_sf, fill="#F3E73B", col="#F3E73B") +
    geom_sf(data=bound, col="darkgrey", linetype="dashed", size=1.1) +
    geom_sf(data=landLayer, fill=c("lightgrey"), col="lightgrey") +
    geom_sf(data=studyArea, fill=NA, col="red", size=1) + 
    ggtitle(plotTitle) 
  
  outPlot <- format_ggplot(rawPlot, plotBbox)
  
  return(outPlot)
}