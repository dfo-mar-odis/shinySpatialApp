# Spatial Reproducible Reporting

# Description

R Shiny App to select search area(s) and generate reproducible reports (R Markdown) in support of the project "Reproducible Reports for Species at Risk: Development of a tool to produce consistent reports on Species at Risk within Predicted Exposure Zones (PEZ) for a variety of threats and activities, focusing on Maritimes Region as a pilot".

# How to request a new Reproducible Report

Code is publicly available via this repository. However, due to issues related to Protected B information, the data are not publicly accessible. As a result, currently, only core members of our team at DFO can generate the reports in response to requests for information internal to DFO. The core team is not, and should not, be perceived as data providers or data custodians.

New report requests are being tracked since October 2022 in https://github.com/orgs/dfo-mar-odis/projects/2/views/26

This new way of requesting/tracking reports will support the process of creating requests to the team, not to individual folks. Furthermore, by using this tracking approach we can keep reporting a list of requests. This helps us justify spending our work time on its development.

To do a new request:

- Make sure you have a GitHub account (or join https://github.com/)
- Access the repo main page: https://github.com/dfo-mar-odis/shinySpatialApp
- On the top left, select the tab 'Issues'
- On the top right, select green tab 'New Issue'
- Select 'RRRequests' and click 'Get Started' 

This step will prompt you to add all the details of your requests. Once that is done, please feel free to send an email to Quentin & Catalina. They will support next steps.

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

5.  Routinely Pull (i.e. git fetch + git merge) from origin/main to deal with any conflicts `git pull origin main`. Optional: If someone else worked on the same branch as you, and had pushed a commit up, you could run `git pull origin my_dev_branch` to sync with those updates.

6.  Once the original issue is fixed, ensure the report can still be generated through the app and that the test suite passes: `testthat::test_local()`

7.  If everything still runs correctly, make a pull request from github website to merge `my_dev_branch` into `main`.

8.  Other developers will then respond to the request and test out the code in their dev branches to make sure there are no issues.

9.  Once all lights are green, the pull request will be accepted and the new code merged into main branch.

## **Updating Data & Metadata**

Note: This is only relevant for DFO project developers that have access to the IN folder. 

When metadata is updated in the preprocessing, you need to resave it before it will update in the rendered reports. 

For this:

- Step 1: Save Data Locally. Run the whole preprocessing script up to the save line. See this link for an example of a pre-processing file: https://github.com/dfo-mar-odis/shinySpatialApp/blob/main/reports/sections/rockweed/rockweed_preprocessing.R

- Step 2: Render Report to make sure everything is working smoothly 

- Step 3: Save Data in the IN folder so all project collaborators can use the most up-to-date information. `remoteFileSavePath` is a variable with the path to the data folder on the IN drive. To save data in the IN folder, replace `localFileSavePath` with `remoteFileSavePath` inside the save function e.g. Replace `save(rockweed_rr, file = file.path(localFileSavePath, "Open/rockweed_rr.RData"))` wtih `save(rockweed_rr, file = file.path(remoteFileSavePath, "Open/rockweed_rr.RData"))`

- Step 4: Write a nice commit in yor branch to announce this update!

## **How to add title levels to the report**

The following provides guidance on how to include titles and subtitles throughout the report to match the formatting. Main modules are presented, with dummy subtitles for data titles and search-area results.


   # **SEARCH RESULTS: ECOLOGICAL DIMENSION**  --> Main title or module, set in /app/templates/*.Rmd

   ## **INFORMATION FROM THE NATIONAL AQUATIC SPECIES AT RISK GEODATABASE**  --> Primary section, set in /app/templates/*.Rmd
   
   ### **Name of the dataset, database, or record**  --> set in individual section Rmd: /reports/sections/sectionName/*.Rmd
   
   ##### *Area-specific search results* --> set in individual section Rmd: /reports/sections/sectionName/*.Rmd
   
   
   
