# COVID-19 Data Visualisation Dashboard

This folder contains R code that generates a dashboard using the COVID-19 data available online
This dashboard allow up-to-date display of the COVID-19 data geospatially using a world map, as well as viewing of the time series of the COVID-date for different countries.

Shiny and Shinydashboard packages are used to generate this dashboard, while sf and rnaturalearth are used to make the maps. ggplot2 is used for the plotting. 

The file app.R contains all the codes for the UI and the server for the shiny app. Data is directly read from an online link. 

The dashboard can be found at https://stabilowl.shinyapps.io/COVID-19-dashboard/
