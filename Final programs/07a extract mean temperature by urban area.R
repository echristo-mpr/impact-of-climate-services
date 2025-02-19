# ***********************************************************************************************************
# Copyright © 2024 Mathematica, Inc. This software was developed by Mathematica as part of the AFOLU GHG Calculator 
# project funded by USAID through Contract No. 51964. This code cannot be copied, distributed or used without 
# the express written permission of Mathematica, Inc.
# ***********************************************************************************************************

# Author: Evan Christo
# Filename: 07a extract mean temperature by urban area
# Date Created: 02/14/2025
# Last Edited: 02/14/2025
# Purpose: Get the mean temperature from 2015-2024

#### SETUP ####

library(rgee)
library(reticulate)
library(dplyr)

#### PREPARE THE URBAN AREAS ####

  # Load the urban areas
  urban_areas <- ee$FeatureCollection('projects/rf-climate-health-51910/assets/urban_areas')
  
  # Create buffers around each urban area
  
  # Initialize buffer radius
  buffer_radius <- 15000 # Radius in meters
  
  # Create buffers
  buffered <- urban_areas$map(function(feature) {
    centroid <- feature$geometry()$centroid();
    buffer <- ee$Feature(centroid$buffer(buffer_radius));
    buffer <- buffer$set('id', feature$get('id'));
    return(buffer)
  })
  
  # Add buffers to the map
  Map$addLayer(buffers)
  
#### PREPARE TEMPERATURE DATA ####
  
  # Import the image collection
  temp <- ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$
    filter(ee$Filter$gte('year', 2015))$ # >= 2015
    filter(ee$Filter$lte('year', 2024))$ # <= 2024
    mean()$ # Get mean across images
    select('temperature_2m')$ # Get temperature band
    subtract(273.15) # Convert to Celsius
  
#### GET MEAN TEMP FOR EACH URBAN AREA ####
  
  # Reduce regions
  temp_by_area <- temp$reduceRegions(
    collection = buffered,
    reducer = ee$Reducer$mean(),
    scale = 11132
  )
  
  # Export to GCS
  task <- ee_table_to_gcs(
    collection = ee$FeatureCollection(temp_by_area),
    description = "07 mean temp by urban area",
    bucket = "ee-51910",
    fileNamePrefix = NULL,
    timePrefix = FALSE,
    fileFormat = "CSV",
    selectors = c("id", "mean")
  )
  task$start()
  ee_monitoring()
  
  
  