# ***********************************************************************************************************
# Copyright © 2024 Mathematica, Inc. This software was developed by Mathematica as part of the AFOLU GHG Calculator 
# project funded by USAID through Contract No. 51964. This code cannot be copied, distributed or used without 
# the express written permission of Mathematica, Inc.
# ***********************************************************************************************************

# Author: Evan Christo
# Filename: 08a extract extreme heat days
# Date Created: 03/14/2025
# Last Edited: 03/14/2025
# Purpose: Get the extreme heat days from 2015-2024 and 2025-2035

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
  Map$addLayer(buffered)

#### PREPARE TEMPERATURE DATA ####

  pleasework <- function(lb) {
  
  # Import the image collection
  hist_temp <- ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$
    filter(ee$Filter$gte('year', 2015))$ # >= 2015
    filter(ee$Filter$lte('year', 2024))$ # <= 2024
    select('temperature_2m') # Get temperature band
    
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
    filter(ee$Filter$date(startDate, endDate))
  
  startDate <- rdate_to_eedate("2025-01-01")
  endDate <- rdate_to_eedate("2036-01-01")
  proj_temp_25_35 <- proj_temp$
    filter(ee$Filter$date(startDate, endDate))
  


    ub <- ub_list[which(lb_list == lb)]
  
    # Define function for getting number of days above 31C
    daysAbove <- function(imageCollec) {
      hotDays <- imageCollec$map(function(image) {
        image <- image$subtract(273.15) # Convert to Celsius
        above <- image$gte(lb)
        below <- image$lt(ub)
        mask <- above$add(below)
        hotDays <- mask$eq(2)
        return(hotDays)
      })
      hotDaysCount <- hotDays$sum()
    }
    
    # Run function for each dataset
    hist_temp <- daysAbove(hist_temp)
    proj_temp_15_24 <- daysAbove(proj_temp_15_24)
    proj_temp_25_35 <- daysAbove(proj_temp_25_35)
    
    # Combine into single image with three bands
    days <- hist_temp$addBands(proj_temp_15_24)$addBands(proj_temp_25_35)$addBands(lb)$addBands(ub)$
      rename(c('hist_15_24', 'proj_15_24', 'proj_25_35', 'lb', 'ub'))
    
  
  #### GET DAYS ABOVE 31 FOR EACH URBAN AREA ####
  
  # Reduce regions
  days_by_area <- days$reduceRegions(
    collection = buffered,
    reducer = ee$Reducer$mean(),
    scale = 11132
  )
  
  filename <- paste0("08 hist and proj days between ", lb, "-", ub, " ")
  
  # Export to GCS
  task <- ee_table_to_gcs(
    collection = ee$FeatureCollection(days_by_area),
    description = filename,
    bucket = "ee-51910",
    fileNamePrefix = NULL,
    timePrefix = FALSE,
    fileFormat = "CSV",
    selectors = c("id", "hist_15_24", "proj_15_24", "proj_25_35", "lb", "ub")
  )
  task$start()
  ee_monitoring()

  }
  
  lb_list <- c(seq(0.0, 45.0, 1.0))
  ub_list <- c(seq(1.0, 45.0), 1000)

  for (lb in lb_list) {
    pleasework(lb)
  }

