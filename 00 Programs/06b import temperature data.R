# ***********************************************************************************************************
# Copyright © 2024 Mathematica, Inc. This software was developed by Mathematica as part of the AFOLU GHG Calculator 
# project funded by USAID through Contract No. 51964. This code cannot be copied, distributed or used without 
# the express written permission of Mathematica, Inc.
# ***********************************************************************************************************

# Author: Evan Christo
# Filename: 06b import temperature data
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

#### IMPORT AND CLEAN ####
  
  # Get objects (SSP245)
  objects <- gcs_list_objects(bucket = "ee-51910", prefix = "06 mean")
  files <- objects$name
  raw <- files %>% 
    lapply(gcs_get_object) %>% # Import files from cloud storage
    bind_rows # Bind files by row
  
  # Get date
  ssp245 <- raw %>% 
    mutate(year = substr(`system:index`, start = 1, stop = 4)) %>% 
    select(!c(`system:index`)) %>%
    rename(ssp245 = mean) %>%
    arrange(id, year) %>%
    relocate(id, year)

#### SAVE ####

  # Save to .r file
  saveRDS(ssp245, file = "02 intermediate/06 temperature data.r")
