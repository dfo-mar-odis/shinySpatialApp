# Spatial Reproducible Reporting

# Description

R Shiny App to select search area(s) and generate reproducible reports (R Markdown) in support of the project "Reproducible Reports for Species at Risk: Development of a tool to produce consistent reports on Species at Risk within Predicted Exposure Zones (PEZ) for a variety of threats and activities, focusing on Maritimes Region as a pilot".

# How to use the App

## Installation

## Set up Revn:
Make sure that [RTools](https://cran.r-project.org/bin/windows/Rtools/) is installed.  

```R
install.packages("devtools")
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

``` r
library(shiny)
runApp("app")
```

## Demo

See [demo.webm](https://github.com/dfo-mar-odis/shinySpatialApp/raw/main/demo.webm).

## Repo structure

-   `app` includes all the files needed to run the application;
-   `app/www` includes image (e.g. logos), the `.css` file, and report preview once generated;
-   `app/R` includes the R files to be loaded (it loads packages and functions);
-   `app/Rmd` includes all the `.Rmd` files needed to generate the report(s);
-   `app/output` includes all outputs once generated: selected geometries as a `.geojson` and the report in all format (the different section are also added).

# **Collaborative Workflow**

## **How to contribute to the project**

1.  Branch from master (e.g. from master create and checkout a new branch: `git checkout -b my_dev_branch` or in RStudio)

2.  Do work in branch, e.g. addressing some issue.

3.  Add and commit changes.

4.  Routinely Push work to the remote version of `my_dev_branch`: `git push -u origin my_dev_branch`

5.  Routinely Pull (i.e. git fetch + git merge) from origin/master to deal with any conflicts `git pull origin master`

6.  Once the original issue is fixed, ensure the report can still be generated through the app and that the test suite passes: `testthat::test_local()`

7.  If everything still runs correctly, make a pull request from github website to merge `my_dev_branch` into `master`.

8.  Other developers will then respond to the request and test out the code in their dev branches to make sure there are no issues.

9.  Once all lights are green, the pull request will be accepted and the new code merged into master branch.
