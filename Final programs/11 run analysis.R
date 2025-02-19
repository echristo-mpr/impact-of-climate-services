# ***********************************************************************************************************
# Copyright © 2024 Mathematica, Inc. This software was developed by Mathematica as part of the AFOLU GHG Calculator 
# project funded by USAID through Contract No. 51964. This code cannot be copied, distributed or used without 
# the express written permission of Mathematica, Inc.
# ***********************************************************************************************************

# Author: Evan Christo
# Filename: 11 run analysis
# Date Created: 11/4/2024
# Last Edited: 02/18/2025
# Purpose: run the analysis

#### SETUP ####

  library(tidyverse)
  library(purrr)
  library(openxlsx)
  
  # Set working directory
  setwd("N:/Project/51910_Rockefeller_PPH_Initiative/DC1/Impact estimate calculator MANUSCRIPT/Programming")
  
  # Clear environment
  rm(list = ls())

#### CALCULATE EXPECTED AND POTENTIAL AVERTED MORTALITY ####

  # Import the analysis file
  df <- readRDS(df, file = "02 intermediate/10 analysis file.r")
  
  # Calculations
  df <- df %>% mutate(
    main_v1 = ho_daily * ssp245 * (th_rr - 1),
    main_v2 = main_v1 * (1 - ih_rr),
    main_v3 = main_v1 * (1 - ih_rr) * (1 + cs_main),
    main_v4 = main_v3 - main_v2,
    low_v1 = ho_daily * ssp245 * (th_rrlb - 1),
    low_v2 = low_v1 * (1 - ih_rrlb),
    low_v3 = low_v1 * (1 - ih_rrlb) * (1 + cs_low),
    low_v4 = low_v3 - low_v2,
    high_v1 = ho_daily * ssp245 * (th_rrub - 1),
    high_v2 = high_v1 * (1 - ih_rrub),
    high_v3 = high_v1 * (1 - ih_rrub) * (1 + cs_high),
    high_v4 = high_v3 - high_v2,
  )
  
  # Replace missings with 0s
  df <- df %>% mutate(
    across(c(main_v1:high_v4), ~ replace_na(., 0))
  )
  
#### SUMMARIZE RESULTS ####
  
  # Get min and max year from dataset
  year_min = min(df$year)
  year_max = max(df$year)

  # Roll-up to the ID level
  results_id <- df %>% group_by(id, adm2, adm1, year) %>% summarise(
    main_v1 = sum(main_v1),
    main_v2 = sum(main_v2),
    main_v3 = sum(main_v3),
    main_v4 = sum(main_v4),
    count_urban_areas = n_distinct(id),
    pop = mean(population),
    mor = mean(ho_yearly)
  ) %>% mutate(
    across(where(is.numeric), round, 2),
    years = paste0(year_min, "-", year_max)
  ) %>% ungroup() %>% mutate(
    main_v1_np = 100 * main_v1 / pop,
    main_v1_nm = 100 * main_v1 / mor,
    main_v4_np = 100 * main_v4 / pop,
    main_v4_nm = 100 * main_v4 / mor
  ) %>% group_by(id, adm2, adm1) %>% summarise(
    main_v1 = sum(main_v1),
    main_v2 = sum(main_v2),
    main_v3 = sum(main_v3),
    main_v4 = sum(main_v4),
    count_urban_areas = mean(count_urban_areas),
    pop = mean(pop),
    mor = mean(mor),
    main_v1_np = mean(main_v1_np),
    main_v1_nm = mean(main_v1_nm),
    main_v4_np = mean(main_v4_np),
    main_v4_nm = mean(main_v4_nm)
  ) %>% ungroup()
  
  # Roll-up to ADM2 level
  results_adm2 <- results_id %>% group_by(adm2, adm1) %>% summarise(
    main_v1 = sum(main_v1),
    main_v2 = sum(main_v2),
    main_v3 = sum(main_v3),
    main_v4 = sum(main_v4),
    count_urban_areas = mean(count_urban_areas),
    pop = sum(pop),
    mor = sum(mor)
  ) %>% ungroup() %>% mutate(
    across(where(is.numeric), round, 2),
    years = paste0(year_min, "-", year_max),
    main_v1_np = 100 * main_v1 / pop,
    main_v1_nm = 100 * main_v1 / mor,
    main_v4_np = 100 * main_v4 / pop,
    main_v4_nm = 100 * main_v4 / mor
  )
  
  # Roll-up to ADM1 level
  results_adm1 <- results_adm2 %>% group_by(adm1) %>% summarise(
    main_v1 = sum(main_v1),
    main_v2 = sum(main_v2),
    main_v3 = sum(main_v3),
    main_v4 = sum(main_v4),
    count_urban_areas = mean(count_urban_areas),
    pop = sum(pop),
    mor = sum(mor)
  )  %>% mutate(
    across(where(is.numeric), round, 2),
    years = paste0(year_min, "-", year_max),
    main_v1_np = 100 * main_v1 / pop,
    main_v1_nm = 100 * main_v1 / mor,
    main_v4_np = 100 * main_v4 / pop,
    main_v4_nm = 100 * main_v4 / mor
  )
    
  # Roll-up to ADM0 level
  results_adm0 <- df %>% group_by(adm0) %>% summarise(
    main_v1 = sum(main_v1),
    main_v2 = sum(main_v2),
    main_v3 = sum(main_v3),
    main_v4 = sum(main_v4),
    count_urban_areas = n_distinct(id)
  )  %>% mutate(
    across(where(is.numeric), round, 2),
    years = paste0(year_min, "-", year_max)
  )
    
  # Roll-up to ADM0-year level
  results_adm0_year <- df %>% group_by(adm0, year) %>% summarise(
    main_v1 = sum(main_v1),
    main_v2 = sum(main_v2),
    main_v3 = sum(main_v3),
    main_v4 = sum(main_v4),
    count_urban_areas = n_distinct(id)
  )  %>% mutate(
    across(where(is.numeric), round, 2)
  )
  
  # Save
  saveRDS(results_id, file = "02 intermediate/11 results id.r")
  saveRDS(results_adm2, file = "02 intermediate/11 results adm2.r")
  saveRDS(results_adm1, file = "02 intermediate/11 results adm1.r")
  saveRDS(results_adm0_year, file = "02 intermediate/11 results adm0 year.r")
  saveRDS(results_adm0, file = "02 intermediate/11 results adm0.r")
  
  # Export to excel
  wb <- createWorkbook()
    
  addWorksheet(wb, "Results - ID")
  writeData(wb, "Results - ID", results_id)
    
  addWorksheet(wb, "Results - ADM1")
  writeData(wb, "Results - ADM1", results_adm1)
  
  addWorksheet(wb, "Results - ADM0")
  writeData(wb, "Results - ADM0", results_adm0)
  
  addWorksheet(wb, "Results - ADM0 year")
  writeData(wb, "Results - ADM0 year", results_adm0_year)
  
#### SENSITIVITY ####
  
    # Pivot long
    df_long <- df %>% select(
      starts_with(c("main", "low", "high")),
      "adm1", "adm0"
    ) %>%
      pivot_longer(
        cols = starts_with(c("main_", "low_", "high_")),   # Select columns with these patterns (main_, low_, high_)
        names_to = c("scenario", "variable"),          # Split column names into two parts
        names_sep = c("_"),
        values_to = "value"                     # Name of the new column for the responses
      )
  
    ### ADM1
  
      # Roll-up to ADM1 level
      sensitivity_adm1 <- df_long %>% group_by(adm1, scenario, variable) %>% summarise(
        value = sum(value),
      ) %>% mutate(
        across(where(is.numeric), round, 2))
      
      # Pivot wider
      sensitivity_adm1_wide <- sensitivity_adm1 %>% 
        pivot_wider(names_from = variable, values_from = value) %>% 
        select(adm1, scenario, v1, v2, v3, v4)
    
    ### ADM2  
      
      # Roll-up to ADM0 level
      sensitivity_adm0 <- df_long %>% group_by(adm0, scenario, variable) %>% summarise(
        value = sum(value),
      ) %>% mutate(
        across(where(is.numeric), round, 2))
      
      # Pivot wider
      sensitivity_adm0_wide <- sensitivity_adm0 %>% 
        pivot_wider(names_from = variable, values_from = value) %>% 
        select(adm0, scenario, v1, v2, v3, v4)
    
    # Export to excel
    addWorksheet(wb, "Sensitivity - ADM1")
    writeData(wb, "Sensitivity - ADM1", sensitivity_adm1_wide)
    addWorksheet(wb, "Sensitivity - ADM0")
    writeData(wb, "Sensitivity - ADM0", sensitivity_adm0_wide)
    
    filename = paste0("03 outputs/11 results ", Sys.Date(), ".xlsx")
    saveWorkbook(wb, filename, overwrite = TRUE)
 