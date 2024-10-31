# ***********************************************************************************************************
# Copyright © 2024 Mathematica, Inc. This software was developed by Mathematica as part of the AFOLU GHG Calculator 
# project funded by USAID through Contract No. 51964. This code cannot be copied, distributed or used without 
# the express written permission of Mathematica, Inc.
# ***********************************************************************************************************

# Author: Evan Christo
# Filename: 05 prepare analysis file
# Date Created: 10/31/2024
# Last Edited: 10/31/2024
# Purpose: import and merge all relevant data

library(googleCloudStorageR)
library(gargle)
library(tidyverse)

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

# POPULATION

  # Population by urban area
  objects <- gcs_list_objects(bucket = "ee-51910", prefix = "02")
  files <- objects$name
  pop2021 <- files %>% lapply(gcs_get_object) %>% bind_rows
  
  # Population growth rate by urban area (from closest urban agglomeration)
  popGrowthRate <- read.csv("01 inputs/all_urbanizationGrowthRate.csv")
  pop <- left_join(popGrowthRate, pop2021, by = c(year = year, id = id)) # Merge by year and agglo ID
  
  # Calculate projected population 
  
  
  # Bring in crude death rate
  crudeMortalityRate <- read.csv("01 inputs/india_crudeMortalityRate.csv")
  pop <- left_join(pop, crudeMortalityRate, by = c(year = year))

# TEMPERATURE

  # Import days above threshold data
  objects <- gcs_list_objects(bucket = "ee-51910", prefix = "03")
  files <- objects$name
  daysAbove <- files %>% lapply(gcs_get_object) %>% bind_rows

  # Calculate days within intervals
  daysAbove <- pivot_wider(daysAbove, names_from = threshold, values_from = mean, names_prefix = "t")
  daysAbove <- daysAbove %>% mutate(
    days_35_36 = t35 - t36,
    days_36_37 = t36 - t37,
    days_37    = t37
  )
  
  # Import and merge in risk ratios by temperature

# PUBLIC HEALTH INTERVENTION
  
  

