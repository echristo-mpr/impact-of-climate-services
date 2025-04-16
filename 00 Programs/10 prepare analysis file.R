# ***********************************************************************************************************
# Copyright © 2024 Mathematica, Inc. This software was developed by Mathematica as part of the AFOLU GHG Calculator 
# project funded by USAID through Contract No. 51964. This code cannot be copied, distributed or used without 
# the express written permission of Mathematica, Inc.
# ***********************************************************************************************************

# Author: Evan Christo
# Filename: 10 prepare analysis file
# Date Created: 10/31/2024
# Last Edited: 02/18/2025
# Purpose: prepare all-cause mortality analysis file

#### SETUP ####

  library(googleCloudStorageR)
  library(gargle)
  library(tidyverse)
  library(purrr)
  library(labelled)

  # Clear environment
  rm(list = ls())
  
  # Set working directory
  setwd("N:/Project/51910_Rockefeller_PPH_Initiative/DC1/Impact estimate calculator MANUSCRIPT/Programming")
  
#### POPULATION ####

  # Import files
  pop2020 <- readRDS(df, file = "02 intermediate/04 population by urban area.r")
  closestAgglo <- readRDS(df, file = "02 intermediate/03 urban areas mapped to urban agglos.r")
  
  # Population growth rate by urban agglomeration
  popGrowthRate <- read.csv("01 inputs/all_urbanizationGrowthRate.csv") %>% 
    rename_with(tolower) %>% # Lower variable names
    rename(country = country.or.area) %>% # Rename variables
    filter(year >= 2020) %>% # Keep 2020 onwards
    pivot_wider(names_from = year, values_from = growthrate, names_prefix = "growthrate_")
  
  # Merge population, closest agglom, and growth rates
  pop <- left_join(pop2020, closestAgglo, by = c("id"))
  pop <- left_join(pop, popGrowthRate, by = c("closestCityCode" = "city.code"))
  
  # Calculate projected population 
  pop_wide <- pop %>% rename(pop_2020 = population)
  pop_wide <- pop_wide %>% mutate(
    pop_2021 = pop_2020 * (1 + growthrate_2020 / 100),
    pop_2022 = pop_2021 * (1 + growthrate_2021 / 100),
    pop_2023 = pop_2022 * (1 + growthrate_2022 / 100),
    pop_2024 = pop_2023 * (1 + growthrate_2023 / 100),
    pop_2025 = pop_2024 * (1 + growthrate_2024 / 100),
    pop_2026 = pop_2025 * (1 + growthrate_2025 / 100),
    pop_2027 = pop_2026 * (1 + growthrate_2026 / 100),
    pop_2028 = pop_2027 * (1 + growthrate_2027 / 100),
    pop_2029 = pop_2028 * (1 + growthrate_2028 / 100),
    pop_2030 = pop_2029 * (1 + growthrate_2029 / 100),
    pop_2031 = pop_2030 * (1 + growthrate_2030 / 100),
    pop_2032 = pop_2031 * (1 + growthrate_2031 / 100),
    pop_2033 = pop_2032 * (1 + growthrate_2032 / 100),
    pop_2034 = pop_2033 * (1 + growthrate_2033 / 100),
    pop_2035 = pop_2034 * (1 + growthrate_2034 / 100),
  )
  
  # Reshape long
  pop <- pop_wide %>% 
    select(!starts_with("growthrate")) %>% 
    pivot_longer(cols = starts_with("pop_"), names_prefix = "pop_", names_to = "year", values_to = "population") %>%
    select(id, year, population) %>%
    mutate(
      population = round(population, 0),
      year = as.numeric(year))
  
  saveRDS(pop, file = "02 intermediate/10 population by urban area.r")
  
#### CRUDE DEATH RATE ####
  
  # Import administrative bounds by ID
  adminBounds <- readRDS(df, file = "02 intermediate/02 urban areas mapped to admin boundaries.r")
  
  # Add two urban areas not in
  added_ids <- data.frame(
    id = c(14,18),
    adm1 = "Gujarat",
    adm2 = "Kachchh"
  )
  adminBounds <- adminBounds %>% bind_rows(added_ids)
  saveRDS(adminBounds, file = "02 intermediate/10 urban areas mapped to admin boundaries.r")
  
  # Import crude death rate
  crudeMortalityRate <- read.csv("01 inputs/india_crudeMortalityRate.csv") %>% 
    select(!source) %>%
    mutate(crudeDeathRate = crudeDeathRate / 1000)
  
  # Merge together by adm1
  cdr <- left_join(adminBounds, crudeMortalityRate, by = c("adm1" = "adm1"))
  
  # Print ADM1s missing a crude death rate
  missing_cdr <- cdr %>% filter(is.na(crudeDeathRate)) %>% select(!crudeDeathRate)
  sort(unique(missing_cdr$adm1))
  sort(unique(crudeMortalityRate$adm1))
  
  # Expand that dataset so each observation has a year
  years <- 2025:2035
  missing_cdr_upd <- missing_cdr %>%
    select(!year) %>%
    cross_join(tibble(year = years)) %>%
    arrange(id, year)
  
  # Merge in country-level for those missing
  cdr_india <- crudeMortalityRate %>% filter(adm1 == "India") %>% select(!adm1)
  missing_merged <- left_join(missing_cdr_upd, cdr_india, by = c("year" = "year"))
  cdr_upd <- cdr %>% 
    filter(!is.na(crudeDeathRate)) %>%
    bind_rows(missing_merged)
  
  # Check which IDs are missing adm1
  missing_adm1 <- cdr_upd %>% filter(is.na(adm1))
  unique(missing_adm1$id)
  
#### BASELINE HEALTH OUTCOME ####
    
  df <- left_join(pop, cdr_upd, by = c("id", "year")) %>% # Combine population and crude mortality dataframes by id and year
    mutate(
      ho_yearly = population * crudeDeathRate,
      ho_daily = ho_yearly / 365.25)  %>% # Calculate baseline health outcome
    filter(year >= 2025) # Only keep years 2025+

#### TEMPERATURE #### 

  # Import temperature
  temp <- readRDS(df, file = "02 intermediate/01 temperature data.r") %>%
    mutate(year = as.numeric(year))
  
  # Merge to analysis file by year and id
  df <- left_join(df, temp, by = c("id", "year"))

#### RISK RATIOS ####
  
  # Import and clean risk ratios by temperature
  heatOnHealth <- read.csv("01 inputs/india_heatOnHealth.csv") %>% 
    select(c("Temp", "RR", "ci.lb", "ci.ub")) %>% 
    rename(
      th_threshold = Temp,
      th_rr = RR,
      th_rrlb = ci.lb,
      th_rrub = ci.ub
    ) %>%
    arrange(th_threshold)
  
  # Add rows for temperatures above 39.5
  new_rows <- data.frame(
    th_threshold = c(40, 41, 43, 45),
    th_rr = heatOnHealth$th_rr[heatOnHealth$th_threshold == 39.5],
    th_rrlb = heatOnHealth$th_rrlb[heatOnHealth$th_threshold == 39.5],
    th_rrub = heatOnHealth$th_rrub[heatOnHealth$th_threshold == 39.5]
  )
  heatOnHealth <- bind_rows(heatOnHealth, new_rows)
  
  # Merge into analysis file
  df <- left_join(df, heatOnHealth, by = c("mean_temp_lb" = "th_threshold"))

#### PUBLIC HEALTH INTERVENTION ####
  
  # Add RR for intervention on health
  df <- df %>% mutate(
    ih_rr = case_when(max_temp_lb == 40 ~ 0.95,
                      max_temp_lb == 41 ~ 0.91,
                      max_temp_lb == 43 ~ 0.90,
                      max_temp_lb == 45 ~ 0.73),
    ih_rrub = case_when(max_temp_lb == 40 ~ 0.73,
                        max_temp_lb == 41 ~ 0.62,
                        max_temp_lb == 43 ~ 0.60,
                        max_temp_lb == 45 ~ 0.29),
    ih_rrlb = case_when(max_temp_lb == 40 ~ 1.00, # 1.22,
                        max_temp_lb == 41 ~ 1.00, # 1.34,
                        max_temp_lb == 43 ~ 1.00, # 1.35,
                        max_temp_lb == 45 ~ 1.00) # 1.81)
  )
  
  # Replace NAs with 1s
  df <- df %>% mutate(
    across(c(ih_rr, ih_rrlb, ih_rrub), ~ replace_na(., 1))
  )
  
#### CLIMATE SERVICE ####
  
  # Add effectiveness % for with and without climate service
  df <- df %>% mutate(
    cs_main = 0.1,
    cs_low = 0.05,
    cs_high = 0.25
  )
  
#### DROP DUPLICATES ####
  
  # Find the duplicates
  duplicates <- df %>% 
    filter(year == 2025) %>% 
    filter(mean_temp_lb == 35) %>%
    filter(max_temp_lb == 0) %>%
    group_by(id, year, mean_temp_lb, mean_temp_ub) %>% 
    filter(n()>1) %>%
    ungroup() %>%
    select(id)
  dups <- as.numeric(duplicates$id)
  
  # Filter out the duplicates
  df <- df %>% filter(!id %in% dups)

#### SAVE THE ANALYSIS FILE ####
  
  # Last touch
  df <- df %>%
    mutate(adm0 = "India") %>%
    relocate(id, year, adm0, adm1, adm2, population)
  
  # Label
  df <- df %>% set_variable_labels(
    id = "Urban area ID",
    year = "Year",
    adm0 = "Administrative boundary 0 - country",
    adm1 = "Adminsitrative boundary 1 - state",
    adm2 = "Adminsitrative boundary 2 - district",
    population = "Population of the urban area",
    crudeDeathRate = "Annual crude death rate per 1,000 people",
    ho_yearly = "Annual adverse health outcome (HO)",
    ho_daily = "Daily adverse health outcome (HO)",
    mean_temp_lb = "Lower bound of mean temperature interval for heat-mortality exposure-response ratios",
    mean_temp_ub = "Upper bound of mean temperature interval for heat-mortality exposure-response ratios",
    max_temp_lb = "Lower bound of max temperature interval for the change in HAP effectiveness based on temperature",
    max_temp_ub = "Upper bound of max temperature interval for the change in HAP effectiveness based on temperature",
    ssp245 = "The number of days within the mean and max temperature intervals for the given year",
    th_rr = "The exposure-response ratio of heat on mortality",
    th_rrlb = "The exposure-response ratio of heat on mortality (lower bound)",
    th_rrub = "The exposure-response ratio of heat on mortality (upper bound)",
    ih_rr = "The change in HAP effectiveness based on temperature",
    ih_rrlb = "The change in HAP effectiveness based on temperature (lower bound)",
    ih_rrub = "The change in HAP effectiveness based on temperature (upper bound)",
    cs_main = "The percent improvement in effectiveness of a HAP when complimented by a HAP",
    cs_low = "The percent improvement in effectiveness of a HAP when complimented by a HAP (lower bound)",
    cs_high = "The percent improvement in effectiveness of a HAP when complimented by a HAP (upper bound)",
  )
  
  # Save
  saveRDS(df, file = "02 intermediate/10 analysis file.r")
