# Wolverine survey design

This repository contains code, data, and an app to facilitate the design of a grid-based camera trap survey for wolverines and other mammals in the Yukon.


## Running the app

The app is located at: https://beaconsproject.shinyapps.io/wolverines (Note that this is an older version; for the latest version, follow the steps below)

The app can also be run from a local machine using the following steps:

  1. Install R (download from r-project.org and follow instructions)
  2. Install the following additional packages:

    install.packages(c("sf","DT","tmap","dplyr","terra","leaflet","shiny","shinydashboard", "magrittr", "tidyverse", "shinybusy"))

  3. Start the Shiny app:

    shiny::runGitHub("beaconsproject/wolverines")


## Preparing the wolverines.gpkg

All of the data for steps 2 and 3 are located in the wolverines/data folder on Dropbox.

### Step 1 - DEM and Landcover

This step requires a free Google Earth Engine (GEE) account. Once created, add the "wolverine_grids" shapefile (code/shp/wolverine_grids.shp) as an Asset in your GEE account and then use the gee_downloads.js script located in the code folder to acquire a 30m resolution DEM ("dem") and landcover map for 2019 ("lcc2019.tif").

### Step 2 - Prepare various layers

Run the code/data_prep1.R script to read, clip, and save various data layers e.g., linear and areal disturbances.

### Step 3 - Calculate cell statistics

Run the code/data_prep2.R script to calculate the area of various layers with each cell e.g., percent linear and areal disturbances.
