# ***********************************************************************************************************
# Copyright © 2024 Mathematica, Inc. This software was developed by Mathematica as part of the AFOLU GHG Calculator 
# project funded by USAID through Contract No. 51964. This code cannot be copied, distributed or used without 
# the express written permission of Mathematica, Inc.
# ***********************************************************************************************************

# Author: Evan Christo
# Filename: 07b import temperature data
# Date Created: 02/14/2025
# Last Edited: 02/14/2025
# Purpose: import and merge all relevant data

#### SETUP ####

  library(googleCloudStorageR)
  library(gargle)
  library(tidyverse)
  library(purrr)
  
  # Clear environment
  rm(list = ls())
  
  # Set up connection to Google Cloud and identify project and bucket
  scope <-c("https://www.googleapis.com/auth/cloud-platform") # Fetch token. See: https://developers.google.com/identity/protocols/oauth2/scopes
  token <- token_fetch(scopes = scope)
  gcs_auth(token = token) # Pass your token to gcs_auth
  proj <- "rf-climate-health-51910"
  buckets <- gcs_list_buckets(proj)
  bucket <- "ee-51910"
  bucket_info <- gcs_get_bucket(bucket)
  gcs_global_bucket(bucket)
  
  # Set working directory
  setwd("N:/Project/51910_Rockefeller_PPH_Initiative/DC1/Impact estimate calculator MANUSCRIPT/Programming")

#### IMPORT AND CLEAN ####
  
  # Get objects (SSP245)
  objects <- gcs_list_objects(bucket = "ee-51910", prefix = "07 hist and proj temp by urban area")
  files <- objects$name
  means <- files %>% 
    lapply(gcs_get_object) %>% # Import files from cloud storage
    bind_rows %>% # Bind files by row
    arrange(id)
  
#### SAVE ####

  # Save to .r file
  saveRDS(means, file = "02 intermediate/07 hist and proj temp by urban area data.r")
  
#### IMPORT AND CLEAN ####
  
  # Get objects (SSP245)
  objects <- gcs_list_objects(bucket = "ee-51910", prefix = "07 hist and proj temp sd by urban area")
  files <- objects$name
  sds <- files %>% 
    lapply(gcs_get_object) %>% # Import files from cloud storage
    bind_rows %>% # Bind files by row
    arrange(id)
  
#### SAVE ####
  
  # Save to .r file
  saveRDS(sds, file = "02 intermediate/07 hist and proj temp sd by urban area data.r")
  
#### IMPORT AND CLEAN ####
  
  # Get objects (SSP245)
  objects <- gcs_list_objects(bucket = "ee-51910", prefix = "07 hist and proj temp min by urban area")
  files <- objects$name
  mins <- files %>% 
    lapply(gcs_get_object) %>% # Import files from cloud storage
    bind_rows %>% # Bind files by row
    arrange(id)
  
#### SAVE ####
  
  # Save to .r file
  saveRDS(mins, file = "02 intermediate/07 hist and proj temp min by urban area data.r")
  
#### IMPORT AND CLEAN ####
  
  # Get objects (SSP245)
  objects <- gcs_list_objects(bucket = "ee-51910", prefix = "07 hist and proj temp max by urban area")
  files <- objects$name
  maxs <- files %>% 
    lapply(gcs_get_object) %>% # Import files from cloud storage
    bind_rows %>% # Bind files by row
    arrange(id)
  
#### SAVE ####
  
  # Save to .r file
  saveRDS(maxs, file = "02 intermediate/07 hist and proj temp max by urban area data.r")
  
#### COMBINE #####
  
  means <- rename_with(means, ~ paste0("mean_", .x), !starts_with("id"))
  sds <- rename_with(sds, ~ paste0("sd_", .x), !starts_with("id"))
  mins <- rename_with(mins, ~ paste0("min_", .x), !starts_with("id"))
  maxs <- rename_with(maxs, ~ paste0("max_", .x), !starts_with("id"))
  
  df <- left_join(means, sds, by = "id")
  df <- left_join(df, mins, by = "id")
  df <- left_join(df, maxs, by = "id")
  
  saveRDS(df, file = "02 intermediate/07 hist and proj temp ALL by urban area data.r")
