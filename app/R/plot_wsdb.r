# This function is created to search a polygon
#
# It adds sightings of the multiple species from the whale sightings database 
# on the map displaying the study area polygon.
# 
# Map has to be plotted outside of the function.

# Species are represented by different symbols.
# Legend with species names is poltted in a separate figure.
#
# Input: - study area polygon (from shape file)
#        - data frame with sightings from the whale sightings database
#        - buf - distance in km from the study area to be plotted on the map
#          this controls the "zoom" of the plot
#
# Written for MSP, Gordana Lazin, September 13, 2019


plot_wsdb <- function(studyArea,wsdb,buf) {
  
  
  # data frame for the whale sightings database
  df=wsdb
  
  # bounding box for studyArea
  bb=as.data.frame(summary(studyArea)$bbox)
  
  # buffer around bounding box
  buf=buf/100
  
  # longitude and latitude limits for the map
  lonLim=c(bb$min[1]-buf, bb$max[1]+buf)
  latLim=c(bb$min[2]-buf, bb$max[2]+buf)
  
  # find sighting points within the box
  lt=which(df$LATITUDE>latLim[1] & df$LATITUDE<latLim[2] )
  lg=which(df$LONGITUDE>lonLim[1] & df$LONGITUDE<lonLim[2] )
  
  # points from the wsdb in the box
  inBox=intersect(lt,lg)
  
  # data in the box
  dfBox=df[inBox,]

  # unique species in the box
  species=as.character(unique(dfBox$COMMONNAME))
  
  # define colours to circle through
  colors=c("darkgoldenrod1","darkgrey","blue", "red","blueviolet", "darkorange2", "cyan","magenta","darkgreen")
  colors=rep(colors, times=ceiling(length(species)/length(colors)))
  colors=colors[1:length(species)]
  
  # define shapes of symbols to use
  shapes=c(16,15,17,18,8)
  shapes=rep(shapes, times=ceiling(length(species)/length(shapes)))
  shapes=shapes[1:length(species)]
  
  # create a legend dataframe that defines symbology for each species
  leg=as.data.frame(cbind(species,colors,shapes))
  leg$shapes=as.numeric(as.character(leg$shapes))
  leg$colors=as.character(leg$colors)
  
  # add sybology to species dataframe, columns colors and shapes
  dfBox=merge(dfBox,leg,by.x="COMMONNAME",by.y="species")
  
  # ADD POINT DATA, only in the box
  points(dfBox$LONGITUDE,dfBox$LATITUDE, col=dfBox$colors, cex=0.8, pch=dfBox$shapes)
  
  # add legend on separate plot
  #par(mar = c(5.1, 1, 4.1,1))
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  legend("topleft", legend=leg$species,col=leg$colors,pch=leg$shapes, cex=0.8,
         inset=-0.01,y.intersp=1.5,bty = "n")
  #par(mar = c(5.1, 4.1, 4.1, 2.1))
  
}