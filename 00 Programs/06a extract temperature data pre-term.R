# ***********************************************************************************************************
# Copyright © 2024 Mathematica, Inc. This software was developed by Mathematica as part of the AFOLU GHG Calculator 
# project funded by USAID through Contract No. 51964. This code cannot be copied, distributed or used without 
# the express written permission of Mathematica, Inc.
# ***********************************************************************************************************

# Author: Evan Christo
# Filename: 06a extract temperature data (pre-term)
# Date Created: 02/14/2025
# Last Edited: 02/14/2025
# Purpose: Get the annual days above a specific temperature threshold for each urban area

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
  buffers <- urban_areas$map(function(feature) {
    centroid <- feature$geometry()$centroid();
    buffer <- ee$Feature(centroid$buffer(buffer_radius));
    buffer <- buffer$set('id', feature$get('id'));
    return(buffer)
  })
  
  # Add buffers to the map
  Map$addLayer(buffers)

#### PREPARE THE IMAGE COLLECTION ####

  # Load the temperature image collection
  startDate <- rdate_to_eedate("2025-01-01")
  endDate <- rdate_to_eedate("2036-01-01")
  temperature <- ee$ImageCollection('NASA/GDDP-CMIP6')$
    filter(ee$Filter$date(startDate, endDate))$
    filter(ee$Filter$eq("model", "ACCESS-CM2"))$
    filter(ee$Filter$eq("scenario", "ssp245"))$
    select(c("tas", "tasmax"))
  
  # Convert to Celsius
  temperature <- temperature$map(function(image) {
    
    # Convert to Celsius
    mean <- image$select('tas')$subtract(273.15)$rename('mean')
    max <- image$select('tasmax')$subtract(273.15)$rename('max')
    
    # Add bands to image
    imageUpd <- image$addBands(mean)$addBands(max)
    imageUpd <- imageUpd$select(c("mean", "max"))
    
    # Return updated image
    return(imageUpd)
    
  })

#### CALCULATE COUNTS BY YEAR ####

  # Create sequence of dates
  year_start <- ee$Date('2025')
  year_end <- ee$Date('2036')
  year_diff <- year_end$difference(year_start, 'year')$round()
  intervals <- ee$List$sequence(0, year_diff$subtract(1), 1)
  year_list <- intervals$map(
    ee_utils_pyfunc(
      function(interval) {
        return(year_start$advance(interval, 'year'))
      }
    )
  )
  
  # Function for calculating and exporting annual days above a temperature threshold
  numberOfDaysAboveThreshold <- function(threshold_mean) {
    
    # Set threshold upper bounds
    threshold_mean_ub = threshold_mean_ub_list[which(threshold_mean_list == threshold_mean)]
    
    # THIS TUTORIAL was a huge help in developing this code: https://www.youtube.com/watch?v=M-cAXJheDQE
    
    # Create image collection of annual days above threshold (each image represents a year)
    daysAbove <- ee$ImageCollection(year_list$map(ee_utils_pyfunc(function(date) {
      
      # Filter image collection by year
      start_date <- ee$Date(date)
      end_date <- start_date$advance(1, 'year')
      filtered <- temperature$filterDate(start_date, end_date)
      
      # For each day, check if temperature is over threshold
      final <- filtered$map(function(image) {
        
        base = image$select("mean")$gte(-100)
        
        image_mean_lb = image$select("mean")$gte(threshold_mean)
        image_mean_ub = image$select("mean")$lt(threshold_mean_ub)
        
        imageUpd = base$mask(image_mean_lb)$mask(image_mean_ub)
        imageFinal = base$eq(imageUpd)
        
        return(imageUpd)
      })
      
      # Take sum across the year
      final <- final$sum()
      
      # Set the year
      final <- final$
        set('system:time_start', start_date$millis())$
        set('system:index', start_date$format('YYYY-MM-dd'))
      
      # Return the final result
      return(final)
      
    })))
    
    # Calculate average daysAbove for each urban area
    daysAboveByArea <- daysAbove$map(function(image) {
      
      # Get the average number of days above threshold by area
      count <- image$reduceRegions(
        collection = buffers,
        reducer = ee$Reducer$mean(),
        scale = 27830
      )
      
      # Copy properties from the image
      count <- count$copyProperties(image)
      
      # Return the feature
      return(count)
    })
    
    # Flatten and export to cloud storage
    flatData <- daysAboveByArea$flatten()
    exportData <- flatData$map(function(feature) {
      return(feature$set('mean_temp_lb', threshold_mean)$
               set('mean_temp_ub', threshold_mean_ub))
    })
    
    # Initialize file name
    filename = paste0("06 mean ", threshold_mean)
    
    # Export to GCS
    task <- ee_table_to_gcs(
      collection = ee$FeatureCollection(exportData),
      description = filename,
      bucket = "ee-51910",
      fileNamePrefix = NULL,
      timePrefix = FALSE,
      fileFormat = "CSV",
      selectors = c("system:index", "id", "mean_temp_lb", "mean_temp_ub", "mean")
    )
    task$start()
    ee_monitoring()
    
  }

#### RUN CALCULATIONS ####

  # Initialize sequence of temperature thresholds
  threshold_mean_list <- c(seq(31.0, 39.0, 1.0))
  threshold_mean_ub_list <- c(seq(32.0, 39.0, 1.0), 1000)
  
  
  # Loop over mean temperature
  for (threshold_mean in threshold_mean_list) {
      
    print(threshold_mean)
    numberOfDaysAboveThreshold(threshold_mean)
      
  }


