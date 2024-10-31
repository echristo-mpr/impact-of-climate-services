# ***********************************************************************************************************
# Copyright © 2024 Mathematica, Inc. This software was developed by Mathematica as part of the AFOLU GHG Calculator 
# project funded by USAID through Contract No. 51964. This code cannot be copied, distributed or used without 
# the express written permission of Mathematica, Inc.
# ***********************************************************************************************************

# Author: Evan Christo
# Filename: 02 population by urban area
# Date Created: 10/30/2024
# Last Edited: 10/30/2024
# Purpose: Get the population of each urban area

library(rgee)
library(reticulate)

# Load the urban areas
urban_areas <- ee$FeatureCollection('projects/rf-climate-health-51910/assets/urban_areas_test')

# Load the population dataset
population <- ee$Image('projects/rf-climate-health-51910/assets/population_test')

# Map and zoom
vizParams <- list(min = 0, max = 16, palette = c("blue", "red"))
Map$addLayer(population, name = "Population", visParams = vizParams)
Map$centerObject(population, zoom = 8)

# Function to get population of each urban area
getPopulation <- function(feature) {
  
  # Get population stats
  populationStats <- population$reduceRegion(
    reducer = ee$Reducer$sum(),
    geometry = feature$geometry(),
    scale = 30,
    maxPixels = 1e9
  )
  
  # Get total population
  totalPopulation <- populationStats$get('b1') # band for population
  
  # Add population as feature property
  return(feature$set(list(population = totalPopulation)))

}

# Run the function
areasWithPopulation <- urban_areas$map(getPopulation)

# Export to cloud storage
task <- ee_table_to_gcs(
  collection = areasWithPopulation,
  description = "02 population by urban area",
  bucket = "ee-51910",
  fileNamePrefix = NULL,
  timePrefix = TRUE,
  fileFormat = "CSV",
  selectors = c("population", "id")
)
task$start()
ee_monitoring()