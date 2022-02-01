# shinySpatialApp 2.0.0-rc1

* Render documents only in HTML (for preview) and print HTML to PDF if desired (see #11).
* Introduce a new module `full_report` to render full reports and review how reports are generated (see 4159a29).
* Add a new feature to look up location on a map (see #13).
* Introduce a new module 'Custom report' that allows for the creation of new R Markdown reports (see #6).
* Hide "spatial projection" option (see #8).
* New section for the full report has been added (see #5).
* Change default options for latitude, longitude and buffer (see #9).
* Rename 'X' "Longitude" and 'Y' "Latitude" (see #15).
* Change default options for latitude, longitude and buffer (see #9).
* Includes DFO functions to create map and extract data. 
* Includes `app/data/` folder where data should be included. 
* `report_pt1_generic_intro_EN.Rmd` includes a example of extraction and map.


# shinySpatialApp 1.0.0 

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