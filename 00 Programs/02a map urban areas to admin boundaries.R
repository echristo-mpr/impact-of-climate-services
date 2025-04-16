# ***********************************************************************************************************
# Copyright © 2024 Mathematica, Inc. This software was developed by Mathematica as part of the AFOLU GHG Calculator 
# project funded by USAID through Contract No. 51964. This code cannot be copied, distributed or used without 
# the express written permission of Mathematica, Inc.
# ***********************************************************************************************************

# Author: Evan Christo
# Filename: 02a map urban areas to admin boundaries
# Date Created: 02/13/2025
# Last Edited: 02/13/2025
# Purpose: import and merge all relevant data

#### SETUP ####

library(rgee)
library(reticulate)
library(dplyr)

#### LOAD AND PREPARE ASSETS ####

  # Load the administrative boundaries (ADM1 and ADM2) and urban areas
  level1 <- ee$FeatureCollection('projects/rf-climate-health-51910/assets/india_adm1')
  level2 <- ee$FeatureCollection('projects/rf-climate-health-51910/assets/india_adm2')
  
  # Load urban areas filtered by India
  urban_areas <- ee$FeatureCollection('projects/rf-climate-health-51910/assets/urban_areas')

  # Filter out any urban areas that intersect with zero boundaries
  filter_adm1 <- urban_areas$filterBounds(level1)

#### MAP AREAS TO BOUNDARIES
  
  # Loop over urban areas
  urban_areas_mapped <- filter_adm1$map(function(urban_area) {
    
    # Get intersection admin boundaries
    intersections_adm1 <- level1$filterBounds(urban_area$geometry())
    intersections_adm2 <- level2$filterBounds(urban_area$geometry())
    
    # For each intersecting ADM1 boundary, get the intersecting area
    areas_with_adm1 <- intersections_adm1$map(function(feature) {
      intersection <- feature$intersection(urban_area)
      intersection_area <- intersection$area()
      feature$set('intersection_area', intersection_area)
    })
    
    # For each intersecting ADM2 boundary, get the intersecting area
    areas_with_adm2 <- intersections_adm2$map(function(feature) {
      intersection <- feature$intersection(urban_area)
      intersection_area <- intersection$area()
      feature$set('intersection_area', intersection_area)
    })
    
    # Get largest intersecting boundary
    majority_adm1 <- areas_with_adm1$sort('intersection_area', FALSE)$first()
    majority_adm2 <- areas_with_adm2$sort('intersection_area', FALSE)$first()
    
    # Create updated feature
    urban_area_upd <- urban_area$
      set('adm1', majority_adm1$get('ADM1'))$
      set('adm2', majority_adm2$get('shapeName'))
    
    # Return
    return(urban_area_upd)
    
  })

  # Export to GCS
  task <- ee_table_to_gcs(
    collection = ee$FeatureCollection(urban_areas_mapped),
    description = '02 urban areas mapped to admin boundaries',
    bucket = "ee-51910",
    fileNamePrefix = NULL,
    timePrefix = FALSE,
    fileFormat = "CSV",
    selectors = c("id", "adm1", "adm2")
  )
  task$start()
  ee_monitoring()
