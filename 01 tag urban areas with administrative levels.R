# ***********************************************************************************************************
# Copyright © 2024 Mathematica, Inc. This software was developed by Mathematica as part of the AFOLU GHG Calculator 
# project funded by USAID through Contract No. 51964. This code cannot be copied, distributed or used without 
# the express written permission of Mathematica, Inc.
# ***********************************************************************************************************
  
# Author: Evan Christo
# Filename: 01 tag urban areas with administrative levels 
# Date Created: 10/29/2024
# Last Edited: 10/30/2024
# Purpose: Map urban areas to their respective administrative regions and export results

library(rgee)
library(reticulate)

# Load the adminsitrative boundaries
level0 <- ee$FeatureCollection('projects/earthengine-legacy/assets/projects/sat-io/open-datasets/geoboundaries/CGAZ_ADM0')
level1 <- ee$FeatureCollection('projects/earthengine-legacy/assets/projects/sat-io/open-datasets/geoboundaries/CGAZ_ADM1')
level2 <- ee$FeatureCollection('projects/earthengine-legacy/assets/projects/sat-io/open-datasets/geoboundaries/CGAZ_ADM2')

# Load the urban areas and agglomerations
urban_areas <- ee$FeatureCollection('projects/rf-climate-health-51910/assets/urban_areas_test')
urban_agglo <- ee$FeatureCollection('projects/rf-climate-health-51910/assets/urban_agglomerations_test')
urban_areas$getInfo()

# Map and zoom
#Map$addLayer(level2, name = "ADM2")
Map$addLayer(urban_areas, name = "Urban areas")
Map$centerObject(urban_areas, zoom = 8)

# Function to map each area to ADM0, ADM1, and ADM2
mapAdminBoundaries <- function(feature) {
  areaGeometry <- feature$geometry()
  
  # Filter the geo-boundaries dataset to find the corresponding administrative areas
  level0_matched <- level0$filterBounds(areaGeometry)
  level1_matched <- level1$filterBounds(areaGeometry)
  level2_matched <- level2$filterBounds(areaGeometry)
  
  # Get the first matched boundary (you can modify this logic if needed)
  level0_features <- level0_matched$toList(1);
  level1_features <- level1_matched$toList(1);
  level2_features <- level2_matched$toList(1);
  level0_feature <- ee$Feature(level0_features$get(0));
  level1_feature <- ee$Feature(level1_features$get(0));
  level2_feature <- ee$Feature(level2_features$get(0));
  
  # Extract administrative information
  adm0 <- level0_feature$get('shapeName')
  adm1 <- level1_feature$get('shapeName')
  adm2 <- level2_feature$get('shapeName')

  # Return the original feature with the admin info added
  return(feature$set(list(
    ADM0 = adm0,
    ADM1 = adm1,
    ADM2 = adm2)))
}

# Map the function over the feature collection
areasWithAdmins <- urban_areas$map(mapAdminBoundaries)

# Export to cloud storage
task <- ee_table_to_gcs(
  collection = areasWithAdmins,
  description = "01 tagged urban areas",
  bucket = "ee-51910",
  fileNamePrefix = NULL,
  timePrefix = TRUE,
  fileFormat = "CSV",
  selectors = c("ADM0", "ADM1", "ADM2", "id")
)
task$start()
ee_monitoring()



