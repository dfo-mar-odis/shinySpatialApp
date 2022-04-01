library(ggplot2)
library(ggspatial)
library(graphics)

# --------------PLOT RR SF-----------------
# Common function for plotting sf data for the reproducible report.
#
# Inputs:
# 1. baseMap: map object that data will be added ontop of, either areaMap or regionMap
# 2. data_sf: sf data to be plotted, can contain either point or polygon data
#    (ideally, pre-clipped to map area with the master_intersect function, using bboxMap, or regionBox)
# 3. Additional parameters passed to subfunctions depending on geometry type
#
# Outputs:
# outplot: plot of the data

plot_rr_sf <- function(baseMap, data_sf, ...) {
  
  if (is.null(data_sf)) {
    return(baseMap)
  }
  if (nrow(data_sf) == 0) {
    return(baseMap)
  }
  
  # only check first row of data_sf to avoid sfc_geometry for mixed poly/multiploys and lines/multilines
  if (inherits(data_sf, "RasterLayer")) {
    
    outPlot <- plot_raster(baseMap, data_sf, ...)
    
  }else if (inherits(sf::st_geometry(data_sf[1, ]), c("sfc_POINT"))) {
    
    outPlot <- plot_points(baseMap, data_sf, ...)
    
  } else if (inherits(sf::st_geometry(data_sf[1, ]), c("sfc_POLYGON", "sfc_MULTIPOLYGON"))) {
    
    outPlot <- plot_polygons(baseMap, data_sf, ...)
    
  } else if (inherits(sf::st_geometry(data_sf[1, ]), c("sfc_LINESTRING", "sfc_MULTILINESTRING"))) {
    
    outPlot <- plot_lines(baseMap, data_sf, ...)
    
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

# helper function to reduce tick mark counts on plots
less_x_ticks <- function(inPlot, tickNum=5) {
  xmin <- inPlot$coordinates$limits$x[1]
  xmax <- inPlot$coordinates$limits$x[2]
  #set outplot axis tick marks
  axisSigFigs <- ceiling(log10(xmax - xmin))
  digits <- ifelse(axisSigFigs < 0, -axisSigFigs, 1)
  breakVec <- round(seq(xmin, xmax, length.out = tickNum),
                    digits = digits)
  outPlot <- inPlot + scale_x_continuous(breaks = breakVec) 
  return(outPlot)
}


# Function for plotting raster data for the reproducible report.
#
# Inputs:
# 1. baseMap = map object, either areaMap or regionMap
# 2. data_sf: raster data to be plotted 
#    (ideally, pre-clipped to map area with the master_intersect function, using bboxMap, or regionBox)
# 3. legendName: string, sets the name of the legend for cases where the attribute is not appropriate. Defaults to
#                the attribute.  
# 
# Created by Quentin Stoyel, December 16, 2021 for reproducible reporting project

plot_raster <- function(baseMap, rasterData, legendName="", bgCutoff=0,
                        rescaleLim=c(0,100), midpoint=50) {
  
  # extract layers to ensure it plots over polygons/study area box
  scaleBarLayer = get_scale_bar_layer(baseMap)
  studyBoxLayer = get_study_box_layer(baseMap)
  watermarkLayer = get_watermark_layer(baseMap)
  
  # axis limits based on baseMap
  axLim = ggplot2::coord_sf(xlim=baseMap$coordinates$limits$x, 
                            ylim=baseMap$coordinates$limits$y, expand=FALSE) 
  
  raster_spdf <- as(rasterData, "SpatialPixelsDataFrame")
  raster_df <- as.data.frame(raster_spdf)
  colnames(raster_df) <- c("value", "x", "y")
  raster_df$value <- round(scales::rescale(raster_df$value, rescaleLim))
  raster_df$value[raster_df$value < bgCutoff] <- NA
  
  dataLayer <- geom_tile(data=raster_df, aes(x=x, y=y, fill=value))
  
  
  legendLayer <-   scale_fill_gradient2(low = 'red', mid="yellow", high = 'green',
                                       na.value = NA, midpoint=midpoint, name=legendName)  

  rasterMap <- baseMap +
    legendLayer +
    dataLayer +
    axLim +
    watermarkLayer +
    studyBoxLayer +
    scaleBarLayer
  
  return(rasterMap) 
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
# 5. colorMap: named list of colours used to set the scale. Names should match factors from attribute col, 
#              values should be color codes.  eg. WhaleCol.
# 6. shapeMap: Similar to colorMap except with values of the R shape codes (eg, 15, 16, ...) instead of colours.
#    
# 
# Created by Quentin Stoyel, September 2, 2021 for reproducible reporting project

plot_points <- function(baseMap, data_sf, attribute="NONE", legendName="", 
                        colorMap=NULL, shapeMap=NULL, size=2.5, shape=20, color="black",
                        continuousAttr=FALSE, minScale=NULL, maxScale=NULL) {
  
  # extract scaleBar layer to ensure it plots over polygons/study area box
  scaleBarLayer = get_scale_bar_layer(baseMap)
  studyBoxLayer = get_study_box_layer(baseMap)
  watermarkLayer = get_watermark_layer(baseMap)
  
  # axis limits based on baseMap
  axLim = ggplot2::coord_sf(xlim=baseMap$coordinates$limits$x, 
                            ylim=baseMap$coordinates$limits$y, expand=FALSE) 
  shapeLayer <- NULL
  
  if (toupper(attribute) == "NONE") {
    # just plot raw data (no colors, shapes, etc)
    dataLayer <- geom_sf(data = data_sf, size = size, shape = shape, color=color) 
    legendLayer <- NULL
  } else {

    if (continuousAttr) {
      legendLayer <- scale_bar_layer(data_sf, attribute, continuousAttr, 
                                     minScale, maxScale, legendName, fill=FALSE)
    } else { 
      data_sf[[attribute]] = as.factor(data_sf[[attribute]])
      if (is.null(colorMap)){
        colorMap <- get_rr_color_map(data_sf[[attribute]])
      } else {
        colorMap <- colorMap[names(colorMap) %in% data_sf[[attribute]]]
        if (!is.null(shapeMap)){
          shapeMap <- shapeMap[names(shapeMap) %in% data_sf[[attribute]]]
          shapeLabels <- names(shapeMap)
          shapeValues <- unname(shapeMap) 
          dataLayer <- geom_sf(data = data_sf, aes(color=!!sym(attribute), shape=!!sym(attribute)), size = size)
          shapeLayer <- scale_shape_manual(labels = shapeLabels, values = shapeValues, name=legendName)  
        }
      }
      legendLayer <- scale_colour_manual(values=colorMap, name=legendName)  
    }
    dataLayer <- geom_sf(data = data_sf, aes(color=!!sym(attribute)), size = size, shape = shape)  
    
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
# 5. outlines: TRUE/FALSE flag, if True outlines will be in black, otherwise outlines will match fill colour of polygon
# 6. colorMap: named list of colours used to set the scale. Names should match factors from attribute col, 
#              values should be color codes.  eg. WhaleCol.
# 7. getColorMap: TRUE/FALSE flag.  If True, plot_polygons will return a named list including the color mapping
#                 used which can be used to match colors between plots.  
#
# Created by Gordana Lazin, July 2, 2021 for reproducible reporting project


plot_polygons <- function(baseMap, polyData, attribute, legendName=attribute,
                          outlines=TRUE, colorMap=NULL, getColorMap=FALSE,
                          labelData=NULL, labelAttribute=NULL, 
                          fillClr="#56B4E9", alpha=1, plotTitle=NULL, 
                          tickNum = NULL, continuousAttr=FALSE, minScale=NULL,
                          maxScale=NULL, lwd=1) {
  
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
  polyLabels <- NULL
  polyOutline <- NULL
  polyFill <- NULL
  titleLayer <- NULL
  
  if (toupper(attribute) == "NONE") { # Case 1: plotting all polygons in one color
    if (!outlines) {
      clr = fillClr
    } 
    polyPlot <- geom_sf(data = polyData, fill = fillClr, col = clr, alpha=alpha)
    
  } else { # Case 2: plotting polygons in different colors based on "attribute" column in the data
    polyAes <- aes(fill = !!sym(attribute))
    
    if (is.null(colorMap)){
      colorMap <- get_rr_color_map(polyData[[attribute]])
    } else {
      colorMap <- colorMap[names(colorMap) %in% polyData[[attribute]]]
    }
    
    polyFill <- scale_bar_layer(polyData, attribute, continuousAttr, minScale,
                                maxScale, legendName, colorMap)
    
    if (outlines) {
      polyPlot <- geom_sf(data=polyData, polyAes, colour=clr, alpha=alpha)
    }
    else {
      polyAes <- modifyList(polyAes, aes(col=!!sym(attribute)))
      polyPlot <- geom_sf(data=polyData, polyAes, alpha = alpha, lwd=lwd)
      polyOutline <- scale_color_manual(values=colorMap, guide="none")  
    }
  }
  
  if(!is.null(labelData)) {
    polyLabels <- ggrepel::geom_label_repel(data = labelData,
                                            aes(label = !!sym(labelAttribute), geometry = geometry),
                                            stat = "sf_coordinates",
                                            min.segment.length = 0
                                            )
  }
  
  if (!is.null(plotTitle)) {
    titleLayer <- ggtitle(plotTitle)
  }
  
    
  polyMap <- baseMap +
      polyPlot +
      polyFill +
      polyOutline +
      polyLabels +
      axLim +
      watermarkLayer +
      studyBoxLayer +
      scaleBarLayer +
      titleLayer
  
  
  if (!is.null(tickNum)){
    polyMap <- less_x_ticks(polyMap, tickNum)
  }  
  
  if (getColorMap) {
    outList <- list(colorMap=colorMap,
                    polyMap=polyMap)
    return(outList)
  } else {
    return(polyMap)  
  }
}


# Function for plotting linestring data for the reproducible report.
#
# Inputs:
# 1. baseMap = map object, either areaMap or regionMap
# 2. data_sf: sf data to be plotted 
#    (ideally, pre-clipped to map area with the master_intersect function, using bboxMap, or regionBox)
# 
plot_lines <- function(baseMap, data_sf, ...) {
  
  # extract scaleBar layer to ensure it plots over polygons/study area box
  scaleBarLayer = get_scale_bar_layer(baseMap)
  studyBoxLayer = get_study_box_layer(baseMap)
  watermarkLayer = get_watermark_layer(baseMap)
  
  # axis limits based on baseMap
  axLim = ggplot2::coord_sf(xlim=baseMap$coordinates$limits$x, 
                            ylim=baseMap$coordinates$limits$y, expand=FALSE) 
  
  # just plot raw data (no colors, shapes, etc)
  dataLayer <- geom_sf(data = data_sf, ...) 
  
  lineMap <- baseMap +
    dataLayer +
    axLim +
    watermarkLayer +
    studyBoxLayer +
    scaleBarLayer
  
  return(lineMap) 
}


# helper function to generate colormap when not specified.  
# RR color scheme is used for first 8 colors, after which the viridis 
# pallette is used. 
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

scale_bar_layer <- function(data_sf, attribute, continuous, minValue, maxValue, legendName, colorMap, fill=TRUE){
  if (continuous){
    if (is.null(minValue)){
      minScale = min(data_sf[[attribute]])
    }
    if (is.null(maxValue)){
      maxScale = max(data_sf[[attribute]])
    }
    if (fill){
      scaleBarLayer <- scale_fill_continuous(type="viridis", name=legendName, 
                                             labels = scales::comma, limits=c(minValue, maxValue))    
    } else {
      scaleBarLayer <- scale_color_continuous(type="viridis", name=legendName, 
                                             labels = scales::comma, limits=c(minValue, maxValue))  
    }
    
  } else {
    data_sf[[attribute]] = as.factor(data_sf[[attribute]])
    if (fill) {
      scaleBarLayer <- scale_fill_manual(values=colorMap, name=legendName)  
    } else {
      scaleBarLayer <- scale_color_manual(values=colorMap, name=legendName)
    }
    
  }
  return(scaleBarLayer)
  
  
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

plot_cetaceans_4grid<-function(finWhale_sf, harbourPorpoise_sf,
                               humpbackWhale_sf, seiWhale_sf, studyArea,
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
  finWhale_sf <- sf::st_crop(finWhale_sf, bboxBuf)
  harbourPorpoise_sf <- sf::st_crop(harbourPorpoise_sf, bboxBuf)
  humpbackWhale_sf <- sf::st_crop(humpbackWhale_sf, bboxBuf)
  seiWhale_sf <- sf::st_crop(seiWhale_sf, bboxBuf)
  
  
  #Fin Whale
  finWhalePlot <- whale_ggplot(finWhale_sf, bound, land, studyArea,
                               "Fin Whale", bboxBuf)
  
  #Harbour Porpoise
  harbourPorpoisePlot <- whale_ggplot(harbourPorpoise_sf, bound, land, studyArea,
                                      "Harbour Porpoise", bboxBuf)
  
  #humpback whale
  humpbackWhalePlot <- whale_ggplot(humpbackWhale_sf, bound, land, studyArea,
                                    "Humpback Whale", bboxBuf)
  
  #Sei Whale
  seiWhalePlot <- whale_ggplot(seiWhale_sf, bound, land, studyArea,
                               "Sei Whale", bboxBuf)
  
  #Arrange all 4 cetaceans into grid
  gridExtra::grid.arrange(finWhalePlot, harbourPorpoisePlot, humpbackWhalePlot,
                          seiWhalePlot,
                          bottom = "",
                          left = "",
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

maps_setup <- function(studyArea, region, areaLandLayer, regionLandLayer, CANborder){
  site <- sf::st_centroid(studyArea)
  # The following defines studyBox geometry "look". studyBox_geom is input into area map or can be added to any map later
  studyBox_geom <- geom_sf(data=studyArea, fill=NA, col="red", size=1)
  
  # The following plots area map using function (output is a list)
  areaMapList <- area_map(studyArea, areaLandLayer, 5, CANborder, studyBox_geom)
  
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

area_map <- function(studyArea, landLayer, bufKm, CANborder, studyBoxGeom) {
  site <- sf::st_centroid(studyArea)
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
    annotation_custom(grid::textGrob("DFO Internal Use Only", rot = rotTheta,
                                     gp = grid::gpar(fontsize = 30, alpha = 0.5,
                                                   col = "grey70",
                                                   fontface = "bold")),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    annotation_scale(location = "bl") +
    theme_bw() +
    labs(x = "",
         y = "",
         col = "")  +
    theme(axis.text.x = element_text(size = 14)) + 
    theme(axis.text.y = element_text(size = 14))
  
  
  # crop to bbox if used:
  if (class(bbox) == "bbox") {
    ggplotOut <- ggplotOut + coord_sf(xlim = c(bbox[["xmin"]], bbox[["xmax"]]),
                                      ylim = c(bbox[["ymin"]], bbox["ymax"]), 
                                      expand = FALSE)
    
  }
  
  return(ggplotOut)
}


# helper function to format individual plots used in whale 4 plot function
whale_ggplot <- function(whale_sf, bound, landLayer, studyArea, plotTitle, plotBbox) {
  
  rawPlot <- ggplot() +
    geom_sf(data=whale_sf, fill="#F3E73B", col="#F3E73B") +
    geom_sf(data=bound, col="darkgrey", linetype="dashed", size=1.1) +
    geom_sf(data=landLayer, fill=c("lightgrey"), col="lightgrey") +
    geom_sf(data=studyArea, fill=NA, col="red", size=1) + 
    ggtitle(plotTitle) 
  
  outPlot <- format_ggplot(rawPlot, plotBbox)
  
  outPlot <- less_x_ticks(outPlot, tickNum = 5)
  
  return(outPlot)
}