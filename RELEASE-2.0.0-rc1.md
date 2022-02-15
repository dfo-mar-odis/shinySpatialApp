---
title: shinySpatialApp -- v2.0.0-rc1
author: Kevin Cazelles & David Beauchesne
---

This document reports our progress on the project [shinySpatialApp](https://github.com/inSilecoInc/shinySpatialApp). All objectives described in the Statement of Work for Reproducible Reports for Species at Risk  (see purchase order n°4500012736) were turned into issues labeled 'deliverable' on the GitHub repository (https://github.com/inSilecoInc/shinySpatialApp/issues). Optional objectifs were also labeled `optional`, the other objectives were labeled `mandatory`. A project 'Version 2' was created to visualize the progress (https://github.com/inSilecoInc/shinySpatialApp/projects/1). Below we provide further details on our achievements. 



# Achievements 

## 'User' tab 

The 'User' tab now includes a new option called 'Indicate for which Region you are generating this report' to select region upfront (see [#10](https://github.com/inSilecoInc/shinySpatialApp/issues/15)). This option allows the user to select between the '*Maritimes Regions*' and the '*Gulf Region*'. Selecting the former provides the user access to the full and custom reports, while the latter only grants access to the custom report.

## 'Geometries' and 'Check' tabs

The two tabs have been merged into one tab named 'View map'. This streamlines the creation, visualization, selection and validation of geometries used to generate the report and makes the interface more intuitive. In streamlining this tab, we addressed multiple issues simultaneously:  

- The validate tab has been modified to allow a user to visualize and explore created geometries more easily; the names of the tabs have also been modified to 'Build geometries' and 'Visualize & validate' to better reflect their purposes (see [#7](https://github.com/inSilecoInc/shinySpatialApp/issues/7), [#12](https://github.com/inSilecoInc/shinySpatialApp/issues/12), and [#14](https://github.com/inSilecoInc/shinySpatialApp/issues/14)). More specifically, the extent of the map is now automatically set to the boundaries of the created geometries when a user opens the 'Visualize & validate' tab. In this tab, a user can now set the map extent to the selected geometries, remove all geometries, and refresh the map extent to the default view centered on Halifax. This means that a user can now easily visualize and zoom into all geometries created or imported in the 'Build geometries' tab from the 'Visualize & validate' tab.

 - New default values have been added for longitude, latitude and buffer as requested (see [#9](https://github.com/inSilecoInc/shinySpatialApp/issues/15)). X and Y fields have renamed Longitude and Latitude (see [#15](https://github.com/inSilecoInc/shinySpatialApp/issues/15)).
Furthermore the spatial projection option is now hidden in a `<details>` tag named "Advanced options" (see [#8](https://github.com/inSilecoInc/shinySpatialApp/issues/8)).

- A new feature has been added to search specific locations and visualize them on the map (see [#13](https://github.com/inSilecoInc/shinySpatialApp/issues/13)). This feature remains available to a user at all times in the 'View map' tab, along with an additional option to refresh the map to the default view centered on Halifax.


## Report tabs

There are now two reports tabs. The previous "Report" tab has been renamed "Full report", has been turned into a module (see https://mastering-shiny.org/scaling-modules.html?q=global#scaling-modules) named `module/full_report.R` and now includes new section (as requested, see [#5](https://github.com/inSilecoInc/shinySpatialApp/issues/5)). A new report tab "Custom report" has been added as a new module (see `modules/customized_report.R`) to include semi-empty Rmd files (see [#6](https://github.com/inSilecoInc/shinySpatialApp/issues/6)).

In order to clarify the rendering of reports we have entirely reviewed the function `R/renderReport()` (renamed 'render_full_report()'). As requested, report are no longer rendered into PDF via R Markdown. Rather, we only render HTML and also the user to build PDF based on the HTML document using `pagedown:chrome_print()` (see [#11](https://github.com/inSilecoInc/shinySpatialApp/issues/11)).  


## Other improvements

<!-- To future dev Reports and full report are modules (see https://mastering-shiny.org/scaling-modules.html?q=module#module-server). Validation using shiny validate https://rstudio.github.io/shinyvalidate/ -->

- `R/zzz.R` has been removed and `global.R` has been introduced (see https://shiny.rstudio.com/articles/scoping.html).


## Comments / Questions for a potential next round of updates

It remains unclear to us what formats are actually needed for the reports. Is HTML enough? Should we include an option to build a `.docx` file? Do you need to generate the report in both English and French at te same time? 

For the custom reports, do you need to include more inputs to be passed to the report?

For the study area request (see [#7](https://github.com/inSilecoInc/shinySpatialApp/issues/7)) it remains unclear to us whether a different type of object needs to be create for use in the report, or if this was solely for visualization purposes. If it's the latter option, we believe that the modification currently proposed address this request without the need for the creation of an additional tab.