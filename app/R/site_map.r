# This function is created specifically for the Aquaculture request purposes
#
# It plots aquaculture site map and zone of influence (PEZ)
#
# Input polygons (from shape files): 
# 1. studyArea- study area or zone of influence (PEZ)
# 2. site- aquaculture site polygon
# 3. land- landmask
# 
# Input parameteras:
# buf - distance in km from the study area to be plotted on the map
# this controls the "zoom" of the plot
#
# Output: map 
#
# Written for MSP, Gordana Lazin, September 13, 2019


site_map <- function(studyArea,site,land,buf) {

  # buf is in km, and now converted to degrees
  buf=buf/100
  #png("pez_and_site.png", width=1616, height=1410)
  
  # bounding box
  bb=as.data.frame(summary(studyArea)$bbox)
  
  # buffer around bounding box
  #buf=0.05
  
  # longitude and latitude limits for the map
  lonLim=c(bb$min[1]-buf, bb$max[1]+buf)
  latLim=c(bb$min[2]-buf, bb$max[2]+buf)
  
  # par(oma = c(0, 0, 0, 0))
  # plot studyArea on the map with buffer around polygon, specified in buf
  map(studyArea,fill=T, col="deepskyblue",xlim=lonLim, ylim=latLim)
  
  # ADD LAND
  map(land,fill=TRUE,col="lightgrey",add=T) 
  
  # ADD SITE
  map(site,fill=TRUE,col="yellow",add=T)
  
  # ADD AXES
  map.axes(las=1, cex.axis=0.8)
  
  # add axis labels - does not want to add y label???
  title(xlab="Longitude [deg]",ylab="Latitude [deg]")
  
  #watermark
  # watermark("For Internal Use Only", col="grey")
  #watermark(show = TRUE, lab = "DFO Internal Use Only")
  

 
}
