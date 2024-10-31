# ***********************************************************************************************************
# Copyright © 2024 Mathematica, Inc. This software was developed by Mathematica as part of the AFOLU GHG Calculator 
# project funded by USAID through Contract No. 51964. This code cannot be copied, distributed or used without 
# the express written permission of Mathematica, Inc.
# ***********************************************************************************************************

# Author: Evan Christo
# Filename: 04 tag urban areas with closest urban agglomeration
# Date Created: 10/31/2024
# Last Edited: 10/31/2024
# Purpose: Tag urban areas to their closest urban agglomeration so we can use the associated population growth rate data

library(rgee)
library(reticulate)
library(dplyr)

# Load the urban areas
urban_areas <- ee$FeatureCollection('projects/rf-climate-health-51910/assets/urban_areas_test')
urban_agglo <- ee$FeatureCollection('projects/rf-climate-health-51910/assets/urban_agglomerations_test')

# Get closest urban areas for each 
closestAgglo <- urban_areas$map(function(area) {
  
  areaGeo <- area$geometry()
  
  # Calculate distance from geometry to all points
  distances <- urban_agglo$map(function(point) {
    pointGeo <- point$geometry()
    distance <- areaGeo$distance(pointGeo)
    return(point$set('distance', distance))
  })
  
  # Find closest
  closestPoint <- distances$sort('distance')$first()
  
  # Get ID of closest point
  closestPointId <- closestPoint$get('Index')
  
  # Add closest point ID to area
  return(area$set('closestUrbanAgglo', closestPointId))
  
})

# Export to cloud storage
task <- ee_table_to_gcs(
  collection = closestAgglo,
  description = "04 closest urban agglo",
  bucket = "ee-51910",
  fileNamePrefix = NULL,
  timePrefix = TRUE,
  fileFormat = "CSV",
  selectors = c("id", "closestUrbanAgglo")
)
task$start()
ee_monitoring()

