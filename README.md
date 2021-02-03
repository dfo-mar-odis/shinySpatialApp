# shinySpatialApp

Shiny Spatial Application for DFO.


# Description 

> The objective of the contract is to assemble a spatial interactive (R Shiny) tool and reproducible reporting (R Markdown) output in support of the project “Reproducible Reports for Species at Risk: Development of a tool to produce consistent reports on Species at Risk within Predicted Exposure Zones (PEZ) for a variety of threats and activities, focusing on Maritimes Region as a pilot”.

Resources : 
- https://docs.google.com/document/d/1oH7z7jITqjPfd2cncf8s0VL83GdWlucd/edit
- https://github.com/AtlanticR/MSP/tree/master/SearchPEZ
- https://david-beauchesne.shinyapps.io/edriversapp/
- https://github.com/BIO-RSG/PhytoFit



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

## Data 

- https://open.canada.ca/data/en/dataset/c094782e-0d6f-4cc0-b5a3-58908493a433


## How to use `shiny`

- https://shiny.rstudio.com/tutorial/written-tutorial/
- https://github.com/r-spatial/mapedit/issues/95
- https://stackoverflow.com/questions/53016404/advantages-of-reactive-vs-observe-vs-observeevent
- https://github.com/dreamRs/shinyWidgets/
- https://github.com/rstudio/shinytableau
- https://github.com/rstudio/shinyvalidate

- https://rstudio.github.io/leaflet/projections.html

<!-- issue https://github.com/rstudio/rmarkdown/pull/572 https://github.com/rstudio/rmarkdown/issues/555 https://yihui.org/issue/-->
