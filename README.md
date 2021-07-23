# Spatial Reproducible Reporting

# Description 

R Shiny App to select search area(s) and generate reproducible reports (R
Markdown) in support of the project “Reproducible Reports for Species at Risk:
Development of a tool to produce consistent reports on Species at Risk within
Predicted Exposure Zones (PEZ) for a variety of threats and activities, focusing
on Maritimes Region as a pilot”.


# How to use the App 

## Set up Revn:
```R
install.packages("renv")
renv::activate()
renv::equip() # loads needed installation libs
renv::restore() # loads all needed packages (may take 15+ mins the first time)
```

Adding a package: 
```R
install.packages("new_package")
renv::snapshot()
```

After adding a new package, commit the updated renv.lock file to source control.


## Run the application

```R 
library(shiny)
runApp("app")
```


## Demo 

See [demo.webm](https://github.com/dfo-mar-odis/shinySpatialApp/raw/main/demo.webm).


## Repo structure

- `app` includes all the files needed to run the application;
- `app/www` includes image (e.g. logos), the `.css` file, and report preview once generated;
- `app/R` includes the R files to be loaded (it loads packages and functions);
- `app/Rmd` includes all the `.Rmd` files needed to generate the report(s);
- `app/output` includes all outputs once generated: selected geometries as a `.geojson` and the report in all format (the different section are also added).