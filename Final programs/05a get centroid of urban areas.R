# ***********************************************************************************************************
# Copyright © 2024 Mathematica, Inc. This software was developed by Mathematica as part of the AFOLU GHG Calculator 
# project funded by USAID through Contract No. 51964. This code cannot be copied, distributed or used without 
# the express written permission of Mathematica, Inc.
# ***********************************************************************************************************

# Author: Evan Christo
# Filename: 03a map urban areas to urban agglos
# Date Created: 02/13/2025
# Last Edited: 02/13/2025
# Purpose: Get centroids of each urban area

#### SETUP ####

library(rgee)
library(reticulate)
library(dplyr)

#### LOAD AND PREPARE ASSETS ####

  # Load the urban areas
  urban_areas <- ee$FeatureCollection('projects/rf-climate-health-51910/assets/urban_areas')

#### GET CENTROID ####
  
  # Get closest urban agglo. for each urban area
  centroids <- urban_areas$map(function(area) {
    
    # Get centroid
    centroid <- area$centroid()
    
    # Add lat and lon as properties
    coords <- centroid$geometry()$coordinates()
    lat <- coords$get(1)
    lon <- coords$get(0)
    centroid_upd <- centroid$set('latitude', lat)$set('longitude', lon)
    
    # Return
    return(centroid_upd)
    
  })
  
  # Export to cloud storage
  task <- ee_table_to_gcs(
    collection = centroids,
    description = "05 urban area centroids",
    bucket = "ee-51910",
    fileNamePrefix = NULL,
    timePrefix = FALSE,
    fileFormat = "CSV",
    selectors = c("id", "latitude", "longitude")
  )
  task$start()
  ee_monitoring()

