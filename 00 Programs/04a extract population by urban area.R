# ***********************************************************************************************************
# Copyright © 2024 Mathematica, Inc. This software was developed by Mathematica as part of the AFOLU GHG Calculator 
# project funded by USAID through Contract No. 51964. This code cannot be copied, distributed or used without 
# the express written permission of Mathematica, Inc.
# ***********************************************************************************************************

# Author: Evan Christo
# Filename: 04a extract population by urban area
# Date Created: 10/30/2024
# Last Edited: 02/13/2025
# Purpose: Get the population of each urban area

#### SETUP ####

  library(rgee)
  library(reticulate)

#### LOAD AND PREPARE ASSETS ####

  # Load the urban areas
  urban_areas <- ee$FeatureCollection('projects/rf-climate-health-51910/assets/urban_areas')

  # Load the population dataset
  population <- ee$ImageCollection('WorldPop/GP/100m/pop')$
    filter(ee$Filter$date('2020-01-01', '2021-01-01'))$
    filter(ee$Filter$eq("country", "IND"))$first()

  # Map and zoom
  vizParams <- list(min = 0, max = 16, palette = c("blue", "red"))
  Map$addLayer(population, name = "Population", visParams = vizParams)
  Map$centerObject(population, zoom = 8)

#### GET POPULATION ####
  
  # Function to get population of each urban area
  getPopulation <- function(feature) {
    
    # Get population stats
    populationStats <- population$reduceRegion(
      reducer = ee$Reducer$sum(),
      geometry = feature$geometry(),
      scale = 100, 
      maxPixels = 1e9
    )
    
    # Get total population
    totalPopulation <- populationStats$get('population') # band for population
    
    # Add population as feature property
    return(feature$set(list(population = totalPopulation)))
    
  }

  # Run the function
  areasWithPopulation <- urban_areas$map(getPopulation)

  # Export to cloud storage
  task <- ee_table_to_gcs(
    collection = areasWithPopulation,
    description = "04 population by urban area",
    bucket = "ee-51910",
    fileNamePrefix = NULL,
    timePrefix = FALSE,
    fileFormat = "CSV",
    selectors = c("population", "id")
  )
  task$start()
  ee_monitoring()