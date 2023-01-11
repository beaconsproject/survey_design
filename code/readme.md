# Preparing the wolverines.gpkg

## Step 1 - DEM and Landcover

This step requires a free Google Earth Engine account. Use the gee_downloads.js script to acquire a 30m resolution DEM ("dem") and landcover map for 2019 ("lcc2019.tif").

## Step 2 - Prepare various layers

Run the data_prep1.R script to read, clip, and save various data layers e.g., linear and areal disturbances.

## Step 3 - Calculate cell statistics

Calculate the area of various layers with each cell e.g., percent linear and areal disturbances.
