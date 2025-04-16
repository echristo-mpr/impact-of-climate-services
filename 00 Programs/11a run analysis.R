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
  raw <- readRDS(df, file = "02 intermediate/10 analysis file.r")
  
  # Get results by id averaged by year
  df_id_year <- raw %>% mutate(
    v1 = ho_daily * ssp245 * (th_rr - 1),
    v2 = v1 * (1 - ih_rr),
    v3 = v2 * (1 + cs_main),
    v4 = v3 - v2,
    v1_pop = v1 / (population / 1000000),
    v4_pop = v4 / (population / 1000000)
  ) %>% select(id, year, adm0, adm1, starts_with("v")) %>%
    group_by(id, year) %>%
    reframe(across(starts_with("v"), ~ sum(.x))) %>%
    ungroup() %>%
    group_by(id) %>%
    reframe(across(starts_with("v"), ~ mean(.x)))
  filename = paste0("02 intermediate/11 results by id annual averages - main.r")
  saveRDS(df_id_year, filename)
  
  by_groups <- c("id", "adm1", "adm0")
  
  # Function
  produceResults <- function(dataframe, err_th, err_ih, err_ci, description) {
    
    # Calculations
    df_id_year <- dataframe %>% mutate(
      v1 = ho_daily * ssp245 * ({{ err_th }} - 1),
      v2 = v1 * (1 - {{ err_ih }}),
      v3 = v2 * (1 + {{ err_ci }}),
      v4 = v3 - v2,
      v1_pop = (ho_daily / (population / 1000)) * ssp245 * ({{ err_th }} - 1),
      v2_pop = v1_pop * (1 - {{ err_ih }}),
      v3_pop = v2_pop * (1 + {{ err_ci }}),
      v4_pop = v3_pop - v2_pop
    ) %>% select(id, year, adm0, adm1, starts_with("v"))
    
    # Loop over by groups
    for (by_group in by_groups) {
      
      # Roll-up to id-level
      df_id <- df_id_year %>% 
        group_by(eval(parse(text = by_group))) %>%
        summarise(
          v1 = sum(v1),
          v2 = sum(v2),
          v3 = sum(v3),
          v4 = sum(v4),
          v1_pop = sum(v1_pop),
          v2_pop = sum(v2_pop),
          v3_pop = sum(v3_pop),
          v4_pop = sum(v4_pop), 
          count_urban_areas = n_distinct(id),
        ) %>% 
        mutate(
          across(where(is.numeric), round, 2)
        )
      
      # Save as .R
      filename = paste0("02 intermediate/11 results by ", by_group, " - ", description, ".r")
      saveRDS(df_id, filename)
      
    }
    
    # Roll-up to adm0-year-level
    df_adm0_year <- df_id_year %>% 
      select(id, adm0, year, starts_with("v")) %>%
      group_by(adm0, year) %>%
      summarise(
        v1 = sum(v1),
        v2 = sum(v2),
        v3 = sum(v3),
        v4 = sum(v4),
        v1_pop = sum(v1_pop),
        v2_pop = sum(v2_pop),
        v3_pop = sum(v3_pop),
        v4_pop = sum(v4_pop), 
        count_urban_areas = n_distinct(id),
      ) %>% 
      mutate(
        across(where(is.numeric), round, 2)
      )
    
    # Save as .R
    filename = paste0("02 intermediate/11 results by adm0 and year - ", description, ".r")
    saveRDS(df_adm0_year, filename)
    
  }
  
  # Produce results and check
  produceResults(raw, th_rr, ih_rr, cs_main, "main")
  check <- readRDS("02 intermediate/11 results by adm0 and year - main.r")
  
  # Sensitivity analyses
  produceResults(raw, th_rrlb, ih_rr, cs_main, "s1a")
  produceResults(raw, th_rrub, ih_rr, cs_main, "s1b")
  produceResults(raw, th_rr, ih_rrlb, cs_main, "s2a")
  produceResults(raw, th_rr, ih_rrub, cs_main, "s2b")
  produceResults(raw, th_rr, ih_rr, cs_low, "s3a")
  produceResults(raw, th_rr, ih_rr, cs_high, "s3b")
  
    # Combine sensitivity results
    compiled <- readRDS("02 intermediate/11 results by adm0 - s1a.r") %>% mutate(
      sensitivity = "s1a"
    )
    for (sens in c("s1b", "s2a", "s2b", "s3a", "s3b")) {
      filename = paste0("02 intermediate/11 results by adm0 - ", sens, ".r")
      hold <- readRDS(filename) %>% mutate(
        sensitivity = sens
      )
      compiled <- compiled %>% bind_rows(hold)
    }
    main <- readRDS("02 intermediate/11 results by adm0 - main.r") %>% mutate(sensitivity = "main")
    compiled <- compiled %>% bind_rows(main)
    sensitivity <- compiled %>% rename(adm0 = `eval(parse(text = by_group))`)
    
  # Envelope analyses
  produceResults(raw, th_rrlb, ih_rrlb, cs_low, "envelope lower")
  produceResults(raw, th_rrub, ih_rrub, cs_high, "envelope upper")
  
    # Combine sensitivity results
    lower <- readRDS("02 intermediate/11 results by adm0 - envelope lower.r") %>% mutate(envelope = "lower")
    upper <- readRDS("02 intermediate/11 results by adm0 - envelope upper.r") %>% mutate(envelope = "upper")
    main <- readRDS("02 intermediate/11 results by adm0 - main.r") %>% mutate(envelope = "main")
    envelopes <- lower %>% bind_rows(main) %>% bind_rows(upper) %>% rename(adm0 = `eval(parse(text = by_group))`)
    
  
  # Export to excel
  wb <- createWorkbook()
  addWorksheet(wb, "Results - ID")
  results_id <- readRDS("02 intermediate/11 results by id - main.r") %>% rename(id = `eval(parse(text = by_group))`)
  writeData(wb, "Results - ID", results_id)
    
  addWorksheet(wb, "Results - ADM1")
  results_adm1 <- readRDS("02 intermediate/11 results by adm1 - main.r") %>% rename(adm1 = `eval(parse(text = by_group))`)
  writeData(wb, "Results - ADM1", results_adm1)
  
  addWorksheet(wb, "Results - ADM0")
  results_adm0 <- readRDS("02 intermediate/11 results by adm0 - main.r") %>% rename(adm0 = `eval(parse(text = by_group))`)
  writeData(wb, "Results - ADM0", results_adm0)
  
  addWorksheet(wb, "Results - ADM0 year")
  results_adm0_year <- readRDS("02 intermediate/11 results by adm0 and year - main.r")
  writeData(wb, "Results - ADM0 year", results_adm0_year)
  
  addWorksheet(wb, "Sensitivity")
  writeData(wb, "Sensitivity", sensitivity)
  
  addWorksheet(wb, "Envelopes")
  writeData(wb, "Envelopes", envelopes)
    
  filename = paste0("03 outputs/11 results ", Sys.Date(), ".xlsx")
  saveWorkbook(wb, filename, overwrite = TRUE)
 