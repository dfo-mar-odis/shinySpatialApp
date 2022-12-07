---
name: Training 3
about: Third Training Ticket
title: Render the report
labels: Training
assignees: ''

---

### Action
Once first two training tickets are done, progressively try rendering the report.

- [ ]  Start by opening the shinyApp:  shiny::runApp("app").  The app is not connected to anything, so don't worry about breaking things/accidentally publishing sensitive data.  Explore it to your hearts content.
- [ ] Check the tick boxes for the terms and conditions
- [ ] Draw a polygon on the map and "save" it
- [ ] "validate" it
- [ ] Generate a section of the report.  The "Habitat" section tends to be pretty quick, so deselect everything except that one and hit "generate report
- [ ] If the report renders, close the shiny app and make sure you can render individual rmarkdowns:  open here::here("reports/sections/ebsa/ebsa_en.rmd") and try rendering it with the "knit" button.

### Context
This is the key deliverable operation for this project, it's important that it can be done smoothly

### Effort
3? Also feel free to edit any of these effort values/tickets as you see fit.  At this point, these values should be an indicator of how hard you should work on a thing if you're stuck.
