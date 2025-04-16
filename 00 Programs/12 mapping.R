# ***********************************************************************************************************
# Copyright © 2024 Mathematica, Inc. This software was developed by Mathematica as part of the AFOLU GHG Calculator 
# project funded by USAID through Contract No. 51964. This code cannot be copied, distributed or used without 
# the express written permission of Mathematica, Inc.
# ***********************************************************************************************************

# Author: Evan Christo
# Filename: 12 mapping
# Date Created: 02/12/2025
# Last Edited: 02/18/2025
# Purpose: update dbf

#### SETUP ####

  library(foreign)
  library(sf)
  library(dplyr)

  # Clear environment
  rm(list = ls())
  
  # Set working directory
  setwd("N:/Project/51910_Rockefeller_PPH_Initiative/DC1/Impact estimate calculator MANUSCRIPT")
  
#### ADM1 results ####

#   # Import ADM1-level results
#   results_adm1 <- readRDS(df, file = "Programming/02 intermediate/11 results adm1.r")
# 
#   # Read in and write the .shp file
#   shp <- st_read("Mapping/01 inputs/geoBoundaries-IND-ADM1_simplified.shp")
#   st_write(shp, dsn = "Mapping/02 intermediate/12 india adm1 with results.shp", append = FALSE)
# 
#   # Update the .dbf file
#   dbf <- read.dbf("Mapping/02 intermediate/12 india adm1 with results.dbf")
#   sort(dbf$ADM1)
#   sort(results_adm1$adm1)
#   dbf_upd <- left_join(dbf, results_adm1, by = c("ADM1" = "adm1")) %>%
#     replace(is.na(.), 0)
#   write.dbf(dbf_upd, "Mapping/02 intermediate/12 india adm1 with results.shp")
#   
# #### ADM2 results ####
#   
#   # Import ADM2-level results
#   results_adm2 <- readRDS(df, file = "Programming/02 intermediate/11 results adm2.r")
#   
#   # Read in and write the .shp file
#   shp <- st_read("Mapping/01 inputs/geoBoundaries-IND-ADM2_simplified.shp")
#   st_write(shp, dsn = "Mapping/02 intermediate/12 india adm2 with results.shp", append = FALSE)
#   
#   # Sleep
#   Sys.sleep(3)
#   
#   # Update the .dbf file
#   dbf <- read.dbf("Mapping/02 intermediate/12 india adm2 with results.dbf")
#   sort(dbf$shapeName)
#   sort(results_adm1$adm2)
#   dbf_upd <- left_join(dbf, results_adm2, by = c("shapeName" = "adm2")) %>%
#     replace(is.na(.), 0)
#   write.dbf(dbf_upd, "Mapping/02 intermediate/12 india adm2 with results.shp")
  
#### ID results (centroid) ####
  
  # Import id-level results averaged by year
  results_id <- readRDS(df, file = "Programming/02 intermediate/11 results by id annual averages - main.r")
  
  # # Read in and write the .shp file
  # shp <- st_read("Mapping/01 inputs/india_urban_areas.shp")
  # st_write(shp, dsn = "Mapping/02 intermediate/12 india id with results.shp", append = FALSE)
  # 
  # # Sleep
  # Sys.sleep(3)
  # 
  # # Update the .dbf file
  # dbf <- read.dbf("Mapping/02 intermediate/12 india id with results.dbf")
  # sort(dbf$id)
  # sort(results_id$id)
  # dbf_upd <- left_join(dbf, results_id, by = c("id" = "id")) %>%
  #   replace(is.na(.), 0)
  # write.dbf(dbf_upd, "Mapping/02 intermediate/12 india id with results.dbf")
  # 
  
  # Import centroid
  centroids <- readRDS(df, file = "Programming/02 intermediate/05 urban areas centroids.r")
  
  # Merge
  export <- left_join(results_id, centroids, by = c("id"))
  write.csv(export, "Mapping/02 intermediate/12 india id centroid with annual averaged results.csv")
  
#### Temperature ####
  
  # Import temp
  temp <- readRDS(file = "Programming/02 intermediate/07 hist and proj temp ALL by urban area data.r")
  
  # Import heat days
  days <- readRDS(file = "Programming/02 intermediate/08 hist and proj days above 31 by urban area data.r")
  days <- days %>% mutate(across(starts_with("days"), ~ .x / 11))
  
  # Merge
  export <- left_join(centroids, temp, by = "id")
  export <- left_join(export, days, by = "id")
  
  # Export
  write.csv(export, "Mapping/02 intermediate/12 india id centroid temp stats.csv")
  
