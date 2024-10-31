# ***********************************************************************************************************
# Copyright © 2024 Mathematica, Inc. This software was developed by Mathematica as part of the AFOLU GHG Calculator 
# project funded by USAID through Contract No. 51964. This code cannot be copied, distributed or used without 
# the express written permission of Mathematica, Inc.
# ***********************************************************************************************************

# Author: Evan Christo
# Filename: 03 days above temperature threshold by urban area
# Date Created: 10/30/2024
# Last Edited: 10/31/2024
# Purpose: Get the annual days above a specific temperature threshold for each urban area

library(rgee)
library(reticulate)
library(dplyr)

# Load the urban areas
urban_areas <- ee$FeatureCollection('projects/rf-climate-health-51910/assets/urban_areas_test')

# Load the temperature image collection
startDate <- rdate_to_eedate("2025-01-01")
endDate <- rdate_to_eedate("2028-01-01")
temperature <- ee$ImageCollection('NASA/GDDP-CMIP6')$
  filter(ee$Filter$date(startDate, endDate))$
  filterBounds(urban_areas)$
  filter(ee$Filter$eq("model", "ACCESS-CM2"))$
  filter(ee$Filter$eq("scenario", "ssp245"))$
  select("tas")

# Grab and visualize the first image
# image <- temperature$first()
# print(image$getInfo())
# bandNames <- image$bandNames()
# print(bandNames$getInfo())
# vizParams <- list(min = 273, max = 313, palette = c("blue", "red"))
# Map$addLayer(image, visParams = vizParams)
# Map$centerObject(urban_areas, zoom = 5)

# Create sequence of dates
year_start <- ee$Date('2025')
year_end <- ee$Date('2028')
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
numberOfDaysAboveThreshold <- function(threshold) {
  
  # THIS TUTORIAL was a huge help in developing this code: https://www.youtube.com/watch?v=M-cAXJheDQE

  # Create image collection of annual days above threshold (each image represents a year)
  daysAbove <- ee$ImageCollection(year_list$map(
    ee_utils_pyfunc(function(date) {
      start_date <- ee$Date(date)
      end_date <- start_date$advance(1, 'year')
      filtered <- temperature$filterDate(start_date, end_date)
      final <- filtered$map(function(image) {
        return(image$subtract(273.15)$gte(threshold))
      })
      final <- final$sum()
      final <- final$
        set('system:time_start', start_date$millis())$
        set('system:index', start_date$format('YYYY-MM-dd'))
      return(final)
    })
  ))
  
  # Calculate average daysAbove for each urban area
  daysAboveByArea <- daysAbove$map(function(image) {
    count <- image$reduceRegions(
      collection = urban_areas,
      reducer = ee$Reducer$mean(),
      scale = 27830
    )
    count <- count$copyProperties(image)
    return(count)
  }) 
  
  filename = paste0("03 days above ", threshold, " by urban area ")
  
  # Flatten and export to cloud storage
  flatData <- daysAboveByArea$flatten()
  exportData <- flatData$map(function(feature) {
    return(feature$set('threshold', threshold))
  })
  task <- ee_table_to_gcs(
    collection = ee$FeatureCollection(exportData),
    description = filename,
    bucket = "ee-51910",
    fileNamePrefix = NULL,
    timePrefix = FALSE,
    fileFormat = "CSV",
    selectors = c("system:index", "threshold", "id", "mean")
  )
  task$start()
  ee_monitoring()
  
}

# Run the function for a sequence of temperature thresholds
threshold_list <- seq(35, 37, 1)
lapply(threshold_list, numberOfDaysAboveThreshold)
