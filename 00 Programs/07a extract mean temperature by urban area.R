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
  # Map$addLayer(buffered)
  
#### PREPARE TEMPERATURE DATA ####
  
  # MEANS #
  
    # Import the image collection
    hist_temp <- ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$
      filter(ee$Filter$gte('year', 2015))$ # >= 2015
      filter(ee$Filter$lte('year', 2024))$ # <= 2024
      mean()$ # Get mean across images
      select('temperature_2m')$ # Get temperature band
      subtract(273.15) # Convert to Celsius
    
    # Import temperature projections
    startDate <- rdate_to_eedate("2015-01-01")
    endDate <- rdate_to_eedate("2036-01-01")
    proj_temp <- ee$ImageCollection('NASA/GDDP-CMIP6')$
      filter(ee$Filter$date(startDate, endDate))$
      filter(ee$Filter$eq("model", "ACCESS-CM2"))$
      filter(ee$Filter$eq("scenario", "ssp245"))$
      select(c("tas")) # Get temperature band
    
    startDate <- rdate_to_eedate("2015-01-01")
    endDate <- rdate_to_eedate("2025-01-01")
    proj_temp_15_24 <- proj_temp$
      filter(ee$Filter$date(startDate, endDate))$
      mean()$ # Get mean across images
      subtract(273.15) # Convert to Celsius
      
    startDate <- rdate_to_eedate("2025-01-01")
    endDate <- rdate_to_eedate("2036-01-01")
    proj_temp_25_35 <- proj_temp$
      filter(ee$Filter$date(startDate, endDate))$
      mean()$ # Get mean across images
      subtract(273.15) # Convert to Celsius
    
    # Combine into single image with three bands
    temp_mean <- hist_temp$addBands(proj_temp_15_24)$addBands(proj_temp_25_35)$
      rename(c('hist_15_24', 'proj_15_24', 'proj_25_35'))
  
  # MIN #
    
    # Import the image collection
    hist_temp <- ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$
      filter(ee$Filter$gte('year', 2015))$ # >= 2015
      filter(ee$Filter$lte('year', 2024))$ # <= 2024
      min()$ # Get mean across images
      select('temperature_2m')$ # Get temperature band
      subtract(273.15) # Convert to Celsius
    
    # Import temperature projections
    startDate <- rdate_to_eedate("2015-01-01")
    endDate <- rdate_to_eedate("2036-01-01")
    proj_temp <- ee$ImageCollection('NASA/GDDP-CMIP6')$
      filter(ee$Filter$date(startDate, endDate))$
      filter(ee$Filter$eq("model", "ACCESS-CM2"))$
      filter(ee$Filter$eq("scenario", "ssp245"))$
      select(c("tas")) # Get temperature band
    
    startDate <- rdate_to_eedate("2015-01-01")
    endDate <- rdate_to_eedate("2025-01-01")
    proj_temp_15_24 <- proj_temp$
      filter(ee$Filter$date(startDate, endDate))$
      min()$ # Get mean across images
      subtract(273.15) # Convert to Celsius
    
    startDate <- rdate_to_eedate("2025-01-01")
    endDate <- rdate_to_eedate("2036-01-01")
    proj_temp_25_35 <- proj_temp$
      filter(ee$Filter$date(startDate, endDate))$
      min()$ # Get mean across images
      subtract(273.15) # Convert to Celsius
    
    # Combine into single image with three bands
    temp_min <- hist_temp$addBands(proj_temp_15_24)$addBands(proj_temp_25_35)$
      rename(c('hist_15_24', 'proj_15_24', 'proj_25_35'))    
  
  # MAX #
    
    # Import the image collection
    hist_temp <- ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$
      filter(ee$Filter$gte('year', 2015))$ # >= 2015
      filter(ee$Filter$lte('year', 2024))$ # <= 2024
      max()$ # Get mean across images
      select('temperature_2m')$ # Get temperature band
      subtract(273.15) # Convert to Celsius
    
    # Import temperature projections
    startDate <- rdate_to_eedate("2015-01-01")
    endDate <- rdate_to_eedate("2036-01-01")
    proj_temp <- ee$ImageCollection('NASA/GDDP-CMIP6')$
      filter(ee$Filter$date(startDate, endDate))$
      filter(ee$Filter$eq("model", "ACCESS-CM2"))$
      filter(ee$Filter$eq("scenario", "ssp245"))$
      select(c("tas")) # Get temperature band
    
    startDate <- rdate_to_eedate("2015-01-01")
    endDate <- rdate_to_eedate("2025-01-01")
    proj_temp_15_24 <- proj_temp$
      filter(ee$Filter$date(startDate, endDate))$
      max()$ # Get mean across images
      subtract(273.15) # Convert to Celsius
    
    startDate <- rdate_to_eedate("2025-01-01")
    endDate <- rdate_to_eedate("2036-01-01")
    proj_temp_25_35 <- proj_temp$
      filter(ee$Filter$date(startDate, endDate))$
      max()$ # Get mean across images
      subtract(273.15) # Convert to Celsius
    
    # Combine into single image with three bands
    temp_max <- hist_temp$addBands(proj_temp_15_24)$addBands(proj_temp_25_35)$
      rename(c('hist_15_24', 'proj_15_24', 'proj_25_35'))
      
  # SD #
    
    # Import the image collection
    hist_temp <- ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$
      filter(ee$Filter$gte('year', 2015))$ # >= 2015
      filter(ee$Filter$lte('year', 2024))$ # <= 2024
      reduce(ee$Reducer$stdDev())$ # Get sd across images
      select('temperature_2m_stdDev') # Get temperature band
    
    # Import temperature projections
    startDate <- rdate_to_eedate("2015-01-01")
    endDate <- rdate_to_eedate("2036-01-01")
    proj_temp <- ee$ImageCollection('NASA/GDDP-CMIP6')$
      filter(ee$Filter$date(startDate, endDate))$
      filter(ee$Filter$eq("model", "ACCESS-CM2"))$
      filter(ee$Filter$eq("scenario", "ssp245"))$
      select(c("tas")) # Get temperature band
    
    startDate <- rdate_to_eedate("2015-01-01")
    endDate <- rdate_to_eedate("2025-01-01")
    proj_temp_15_24 <- proj_temp$
      filter(ee$Filter$date(startDate, endDate))$
      reduce(ee$Reducer$stdDev()) # Get sd across images
    
    startDate <- rdate_to_eedate("2025-01-01")
    endDate <- rdate_to_eedate("2036-01-01")
    proj_temp_25_35 <- proj_temp$
      filter(ee$Filter$date(startDate, endDate))$
      reduce(ee$Reducer$stdDev()) # Get sd across images
    
    # Combine into single image with three bands
    temp_sd <- hist_temp$addBands(proj_temp_15_24)$addBands(proj_temp_25_35)$
      rename(c('hist_15_24', 'proj_15_24', 'proj_25_35'))
    
#### GET MEAN TEMP FOR EACH URBAN AREA ####
  
  # Reduce regions
  mean_by_area <- temp_mean$reduceRegions(
    collection = buffered,
    reducer = ee$Reducer$mean(),
    scale = 11132
  )
    
  # Reduce regions
  mean_by_area <- temp_sd$reduceRegions(
    collection = buffered,
    reducer = ee$Reducer$mean(),
    scale = 11132
  )
  
  # Reduce regions
  min_by_area <- temp_min$reduceRegions(
    collection = buffered,
    reducer = ee$Reducer$mean(),
    scale = 11132
  )
  
  # Reduce regions
  max_by_area <- temp_max$reduceRegions(
    collection = buffered,
    reducer = ee$Reducer$mean(),
    scale = 11132
  )
    
  # # Export to GCS
  # task <- ee_table_to_gcs(
  #   collection = ee$FeatureCollection(mean_by_area),
  #   description = "07 hist and proj temp by urban area",
  #   bucket = "ee-51910",
  #   fileNamePrefix = NULL,
  #   timePrefix = FALSE,
  #   fileFormat = "CSV",
  #   selectors = c("id", "hist_15_24", "proj_15_24", "proj_25_35")
  # )
  # task$start()
  # ee_monitoring()
  # 
  # # Export to GCS
  # task <- ee_table_to_gcs(
  #   collection = ee$FeatureCollection(mean_by_area),
  #   description = "07 hist and proj temp sd by urban area",
  #   bucket = "ee-51910",
  #   fileNamePrefix = NULL,
  #   timePrefix = FALSE,
  #   fileFormat = "CSV",
  #   selectors = c("id", "hist_15_24", "proj_15_24", "proj_25_35")
  # )
  # task$start()
  # ee_monitoring()
  
  # Export to GCS
  task <- ee_table_to_gcs(
    collection = ee$FeatureCollection(min_by_area),
    description = "07 hist and proj temp min by urban area",
    bucket = "ee-51910",
    fileNamePrefix = NULL,
    timePrefix = FALSE,
    fileFormat = "CSV",
    selectors = c("id", "hist_15_24", "proj_15_24", "proj_25_35")
  )
  task$start()
  ee_monitoring()
  
  # Export to GCS
  task <- ee_table_to_gcs(
    collection = ee$FeatureCollection(max_by_area),
    description = "07 hist and proj temp max by urban area",
    bucket = "ee-51910",
    fileNamePrefix = NULL,
    timePrefix = FALSE,
    fileFormat = "CSV",
    selectors = c("id", "hist_15_24", "proj_15_24", "proj_25_35")
  )
  task$start()
  ee_monitoring()
  
  