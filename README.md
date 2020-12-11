# shinySpatialApp

Shiny Spatial Application for DFO.


# To be done


See: 
- https://docs.google.com/document/d/1oH7z7jITqjPfd2cncf8s0VL83GdWlucd/edit
- https://github.com/AtlanticR/MSP/tree/master/SearchPEZ
- https://david-beauchesne.shinyapps.io/edriversapp/
- https://github.com/BIO-RSG/PhytoFit

- [ ] To allow the user to use shapefiles / raster files as inputs
- [ ] To allow the user to choose the databases to be used 
- [ ] To perform an intersect
- [ ] To generate a dynamic report


# How to use the App 

## Installation 

```R 
install.packages("remotes")
remotes::install_deps()
```


## Run the application

```R 
library(shiny)
runApp("app")
```


<!-- add screencast? -->


# Resources

## How to use `shiny`

- https://shiny.rstudio.com/tutorial/written-tutorial/
- https://github.com/r-spatial/mapedit/issues/95
- https://stackoverflow.com/questions/53016404/advantages-of-reactive-vs-observe-vs-observeevent
- https://github.com/dreamRs/shinyWidgets/
- https://github.com/rstudio/shinytableau
- https://github.com/rstudio/shinyvalidate