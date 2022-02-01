---
title: shinySpatialApp -- v2.0.0-rc1
author: Kevin Cazelles & David Beauchesne
---

This document reports our progress on the project [shinySpatialApp](https://github.com/inSilecoInc/shinySpatialApp). All objectives described in the Statement of Work for Reproducible Reports for Species at Risk  (see purchase order n°4500012736) were turned into issues labeled 'deliverable' on the GitHub repository (https://github.com/inSilecoInc/shinySpatialApp/issues). Optional objectifs were also labeled `optional`, the other objectives were labeled `mandatory`. A project 'Version 2' was created to visualize the progress (https://github.com/inSilecoInc/shinySpatialApp/projects/1). Below we provide further details on our achievements. 



# Achievements 

## 'User' tab 

<!-- @David -->
The 'User' tab now includes a new section XXX to select region upfront (see [#10](https://github.com/inSilecoInc/shinySpatialApp/issues/15)).

## 'Geometries' and 'Check' tabs

<!-- @David -->
The two tabs have been merged into one tab names XXX. This streamlines XXX and makes the interface more intuitive.


New default values have been added for longitude, latitude and buffer as requested (see [#9](https://github.com/inSilecoInc/shinySpatialApp/issues/15)). X and Y fields have renamed Longitude and Latitude (see [#15](https://github.com/inSilecoInc/shinySpatialApp/issues/15)).
Furthermore the spatial projection option is now hidden in a `<details>` tag named "Advanced options" (see [#8](https://github.com/inSilecoInc/shinySpatialApp/issues/8)).

A new feature has been added to search specific locations and visualize them on the map (see [#13](https://github.com/inSilecoInc/shinySpatialApp/issues/13)).



## Report tabs

There are now two reports tabs. The previous "Report" tab has been renamed "Full report", has been turned into a module (see https://mastering-shiny.org/scaling-modules.html?q=global#scaling-modules) named `module/full_report.R` and now includes new section (as requested, see [#5](https://github.com/inSilecoInc/shinySpatialApp/issues/5)). A new report tab "Custom report" has been added as a new module (see `modules/customized_report.R`) to include semi-empty Rmd files (see [#6](https://github.com/inSilecoInc/shinySpatialApp/issues/6)).

In order to clarify the rendering of reports we have entirely reviewed the function `R/renderReport()` (renamed 'render_full_report()'). As requested, report are no longer rendered into PDF via R Markdown. Rather, we only render HTML and also the user to build PDF based on the HTML document using `pagedown:chrome_print()` (see [#11](https://github.com/inSilecoInc/shinySpatialApp/issues/11)).  




## Other improvements

<!-- To fiture dev Reports and ful repott are modules (see https://mastering-shiny.org/scaling-modules.html?q=module#module-server). Validatation suing shuiny valiudate https://rstudio.github.io/shinyvalidate/ -->

- `R/zzz.R` has been removed and `global.R` has been introduced (see https://shiny.rstudio.com/articles/scoping.html).




## Comments / Questions for a potential next round of updates

It remains unclear to us what formats are actually needed for the reports. Is HTML enough? Should we include an option to build a `.docx` file? Do you need to generate the report in both English and French at te same time? 

For the custom reports, do you need to include more inputs to be passed to the report?