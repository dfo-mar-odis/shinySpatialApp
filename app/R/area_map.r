# This function produces a map of the area surrounding a box of the study area using ggplot.
# The extent of the area to plot around the studyArea is defined by "buf" parameter in km (sets "zoom")
# This function was created for searchPEZ and it is used as a "basemap" for other plots
#
# Inputs:
# 1. studyArea: polygon of the study area (sf object, defined by the user in the shiny app)
# 2. site: polygon of a study site (like aquaculture site);for now it is a centroid (a point, centre of studyArea)
#    in the past we plotted the site polygon and site is kept for now as a placeholder for the future
# 3. land_layer: polygon developed for Atlantic canada, could be 10K or 50K scale (preloaded)
# 4. buf: how many km around the study area to plot (acts like "zoom")
# 5. CANborder: Canada border, including water (preloaded)
# 6. studyBox_geom: geometry portraying study box (defines the "look" of the study box, defined in the main script)
#
# Outputs: list containing 2 items
# 1. map, that can be used as a basemap for adding data layers
# 2. bounding box of the map that can be used for cropping datasets
#
# Written by Gordana Lazin for reproducibble reporting project, April 12, 2021
# ggplot map developed by Greg Puncher, winter/spring 2021



area_map <- function(studyArea,site,land_layer,buf, CANborder, studyBox_geom) {
  
  # buf is in km, and now converted to degrees
  buf=buf/100
  
  # bounding box for study area
  bbox=st_bbox(studyArea)
  
  # create bounding box for buffer (plot area)
  bboxBuf=bbox
  
  bboxBuf["xmin"]=(bbox$xmin)-buf
  bboxBuf["xmax"]<-(bbox$xmax)+buf
  bboxBuf["ymin"]<-(bbox$ymin)-buf*0.72
  bboxBuf["ymax"]<-(bbox$ymax)+buf*0.72
  
  # convert buffer to polygon
  # pp=st_as_sfc(bboxBuf,crs=4326)
 
  
  # subset land to plot area to speed up plotting
  land=st_crop(land_layer,bboxBuf)
 
  # subset US-Canad boundary to plot area to speed up plotting
  bound=st_transform(CANborder,crs=4326) # to fix: currently the projection is NAD83
  bound=st_crop(bound,bboxBuf)
  
  # make a plot and write it to m
  m<-ggplot()+
    geom_sf(data=site,fill="yellow",col="black", size=0.6)+
    geom_sf(data=bound, col = "darkgrey", linetype = "dashed", size = 1.1)+ # creates US boundary line, 200 nm limit
    geom_sf(data=land,fill=c("lightgrey"), col="black", size=0.7)+
    watermark(show = TRUE, lab = "DFO Internal Use Only")+
    annotation_scale(location="bl")+
    theme_bw()+
    eval(studyBox_geom)+
    coord_sf(xlim = c(bboxBuf["xmin"], bboxBuf["xmax"]), ylim = c(bboxBuf["ymin"], bboxBuf["ymax"]),expand=FALSE)+
    labs(x="Longitude", y="Latitude", col="")+
    theme(axis.title.y = element_text(size = 13))+
    theme(axis.title.x = element_text(size = 13))
    
  
  outList=list(m,bboxBuf)
    
  return(outList)
  
}