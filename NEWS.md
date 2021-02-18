# devel

# Version 1.0.0 

The application have been completed on February 18th, 2021. The application
allows the user to select searching areas (4 methods to do so have been
implemented) and to generate a customized markdown report that is rendered in
the format required. 

The repository structure is as follows: 
- `app` includes all the files needed to run the application;
- `app/www` includes image (e.g. logos), the `.css` file, and report preview once generated;
- `app/R` includes the R files to be loaded (it loads packages and functions);
- `app/Rmd` includes all the `.Rmd` files needed to generate the report(s);
- `app/output` includes all outputs once generated: selected geometries as a `.geojson` and the report in all format (the different section are also added).