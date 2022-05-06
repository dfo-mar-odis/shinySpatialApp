# Spatial Reproducible Reporting

# Description

R Shiny App to select search area(s) and generate reproducible reports (R Markdown) in support of the project "Reproducible Reports for Species at Risk: Development of a tool to produce consistent reports on Species at Risk within Predicted Exposure Zones (PEZ) for a variety of threats and activities, focusing on Maritimes Region as a pilot".

# How to use the App

## Installation

``` r
install.packages("remotes")
remotes::install_deps()
```

Adding a package:

``` r
usethis::use_package("packageName")
```

After adding a new package, commit the updated description file to source control.

## Download necessary data

After making sure that you have the necessary permissions to access the data, run:

``` r
source(here::here("reports/R/dataFunctions.R"))
copy_rdata_files()
```

## Run the application

``` r
shiny::runApp("app")
```

## Demo

See [demo.webm](https://github.com/dfo-mar-odis/shinySpatialApp/raw/main/demo.webm).

## Repo structure

-   `config.R` contains key configuration parameters, including the region, file paths, and the list of Open Data records checked by the github action;
-   `app` includes all the files needed to run the application;
-   `app/www` includes image (e.g. logos), the `.css` file, and report preview once generated;
-   `app/data` contains the local versions of all data used in running the application;
-   `app/R` includes the R scripts used by the shiny app;
-   `app/templates` includes the base `.Rmd` files for each report section, split by language;
-   `app/output` includes all outputs once generated: selected geometries as a `.geojson` and the report in all format (the different section are also added);
-   `app/modules` includes RShiny module files;

-   `reports` includes all the files needed to render the rmd reports;
-   `reports/dataprocessing` contains the common preprocessing scripts to prepare data for use in the application, as well as the scripts used in github actions;
-   `reports/sections` includes the rmarkdown and preprocessing script for each subsection of the report;
-   `reports/sections/template` contains a simple template with basic functionality;
-   `reports/R/` contains the R functions needed in the rmd to generate the reports;
-   `tests/testthat` contains the unit test scripts that are run when test_local() is called.

# **Collaborative Workflow**

## **How to contribute to the project**

1.  Branch from main (e.g. from main create and checkout a new branch: `git checkout -b my_dev_branch` or in RStudio)

2.  Do work in branch, e.g. addressing some issue.

3.  Add and commit changes.

4.  Routinely Push work to the remote version of `my_dev_branch`: `git push -u origin my_dev_branch`

5.  Routinely Pull (i.e. git fetch + git merge) from origin/main to deal with any conflicts `git pull origin main`

6.  Once the original issue is fixed, ensure the report can still be generated through the app and that the test suite passes: `testthat::test_local()`

7.  If everything still runs correctly, make a pull request from github website to merge `my_dev_branch` into `main`.

8.  Other developers will then respond to the request and test out the code in their dev branches to make sure there are no issues.

9.  Once all lights are green, the pull request will be accepted and the new code merged into main branch.

## **How to add content to the report**

The following provides guidance on how to include titles and subtitles throughout the report to match the formatting. Main modules are presented, with dummy subtitles for data titles and search-area results.


   # **SEARCH RESULTS: ECOLOGICAL DIMENSION**  --> Main title or module, set in /app/templates/*.Rmd

   ## **INFORMATION FROM THE NATIONAL AQUATIC SPECIES AT RISK GEODATABASE**  --> Primary section, set in /app/templates/*.Rmd
   
   ### **Name of the dataset, database, or record**  --> set in individual section Rmd: /reports/sections/sectionName/*.Rmd
   
   ##### *Area-specific search results* --> set in individual section Rmd: /reports/sections/sectionName/*.Rmd
