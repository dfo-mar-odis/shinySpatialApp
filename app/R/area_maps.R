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
                  "site" =site,
                  "region" = region,
                  "areaMap" = areaMap,
                  "bboxMap" = bboxMap,
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
  buf <- bufKm / 100
  
  # bounding box for study area
  bbox <- sf::st_bbox(studyArea)
  
  # create bounding box for buffer (plot area)
  bboxBuf <- bbox
  
  bboxBuf["xmin"] <- (bbox$xmin) - buf
  bboxBuf["xmax"] <- (bbox$xmax) + buf
  bboxBuf["ymin"] <- (bbox$ymin) - buf * 0.72
  bboxBuf["ymax"] <- (bbox$ymax) + buf * 0.72
  

  # crop land to plot area to speed up plotting
  land <- sf::st_crop(landLayer, bboxBuf)
  
  # crop US-Canad boundary to plot area to speed up plotting
  bound <- sf::st_crop(CANborder, bboxBuf)
  
  # configure the plot
  outPlot <- ggplot() + 
    geom_sf(data = site, fill = "yellow", col = "black", size = 0.6) +
    geom_sf(data = bound, col = "darkgrey", linetype = "dashed", size = 1.1) + # creates US boundary line, 200 nm limit
    geom_sf(data = land, fill = c("lightgrey"), col = "black", size = 0.7) +
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
    geom_sf(data = land, fill = c("lightgrey"), col = "black", size = 0.7) +
    geom_sf(data = studyArea, fill = NA, col = "red", size = 1)
  
  outPlot <- format_ggplot(rawPlot, regionBbox)
  
  return(outPlot)
}


#-----------Format Ggplot ----------
# Function that takes a ggplot object as input and adds preset formatting, 
# and axis labels of latitude and longitude, allows all plots
# to have a consistent style.  

format_ggplot <- function(ggplotIn, bbox = FALSE) {
  
  ggplotOut <- ggplotIn + watermark(show = TRUE, lab = "DFO Internal Use Only")+
    annotation_scale(location = "bl")+
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
