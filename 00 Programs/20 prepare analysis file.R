# ***********************************************************************************************************
# Copyright © 2024 Mathematica, Inc. This software was developed by Mathematica as part of the AFOLU GHG Calculator 
# project funded by USAID through Contract No. 51964. This code cannot be copied, distributed or used without 
# the express written permission of Mathematica, Inc.
# ***********************************************************************************************************

# Author: Evan Christo
# Filename: 20 prepare analysis file
# Date Created: 02/14/2025
# Last Edited: 02/18/2025
# Purpose: prepare pre-term analysis file

#### SETUP ####

  library(googleCloudStorageR)
  library(gargle)
  library(tidyverse)
  library(purrr)
  library(readxl)
  library(zoo)
  library(labelled)
  
  # Clear environment
  rm(list = ls())
  
  # Set working directory
  setwd("N:/Project/51910_Rockefeller_PPH_Initiative/DC1/Impact estimate calculator MANUSCRIPT/Programming")

#### BIRTHS ####

  # Import population by year
  pop <- readRDS(df, file = "02 intermediate/10 population by urban area.r") %>%
    filter(year >= 2025)
  adminBounds <- readRDS(df, file = "02 intermediate/10 urban areas mapped to admin boundaries.r")
  excel_sheets("01 inputs/preterm_data - Copy.xlsx")
  
  # Import urban birth rates
  br_state <- read_excel("01 inputs/preterm_data - Copy.xlsx", sheet = "Urban birth rate") %>%
    select(state, br_urban_2016) %>%
    rename(br = br_urban_2016) %>%
    mutate(
      year = 2016
    )
  
  # Import national birth rates
  br_national <- read_excel("01 inputs/preterm_data - Copy.xlsx", sheet = "National birth rate") %>%
    rename_with(tolower) %>%
    select(time, value, variant) %>%
    rename(
      br = value,
      year = time
    ) %>%
    mutate(
      variant = case_when(
        variant == "95% lower bound" ~ "_lb",
        variant == "95% upper bound" ~ "_ub",
        variant == "Median" ~ ""
      )
    ) %>%
    pivot_wider(
      names_from = variant,
      names_prefix = "br",
      values_from = br
    ) %>%
    mutate(
      br_pct_change = (br-lag(br))/br,
      br_lb_pct_change = (br_lb-lag(br_lb))/br_lb,
      br_ub_pct_change = (br_ub-lag(br_ub))/br_ub,
    ) %>%
    select(year, ends_with("pct_change"))

  # Prepare state-level birth rate for merge
  years <- 2016:2035
  br_state_upd <- br_state %>%
    select(!year) %>%
    cross_join(tibble(year = years)) %>%
    arrange(state, year) %>%
    mutate(
      br = case_when(
        year == 2016 ~ br,
        TRUE ~ NA 
    ))
  
  # Merge
  br_upd <- left_join(br_state_upd, br_national, by = c("year")) %>%
    relocate(state, year) %>%
    mutate(br_upd = br)
  
  # Fill birth rates for each year based on the percent change
  br_upd <- br_upd %>%
    group_by(state) %>%
    mutate(
      br_upd = {
        for (i in 2:length(br_upd)) {  # Start from the second year
          if (is.na(br_upd[i])) {  # Only calculate if the birth rate is NA
            br_upd[i] <- br_upd[i-1] * (1 + br_pct_change[i-1])  # Calculate based on previous year
          }
        }
        br_upd  # Return the updated birth_rate
      }
    ) %>%
    ungroup()
  
  # Update adm1 names to match across datasets
  sort(unique(adminBounds$adm1))
  sort(unique(br_upd$state))
  br_upd <- br_upd %>% mutate(
    state = case_when(
      state == "A & N Island" ~ "Andaman and Nicobar Islands",
      state == "D & N Haveli" ~ "Dadra and Nagar Haveli and Daman and Diu",
      state == "Daman & Diu" ~ "Dadra and Nagar Haveli and Daman and Diu",
      state == "Jammu & Kashmir" ~ "Jammu and Kashmir",
      state == "Tamil" ~ "Tamil Nadu",
      TRUE ~ state
    )
  ) %>% group_by(state, year) %>%
    summarise(
      br = mean(br_upd)
    ) %>% ungroup()
  
  # Merge with population
  df <- left_join(pop, adminBounds, by = c("id"))
  df <- left_join(df, br_upd, by = c("year" = "year", "adm1" = "state"))
  
  # Fix records missing birth rates, give them national average
  missing_br <- df %>% filter(is.na(br)) %>% select(!br)
  india_br <- br_upd %>% filter(state == "India") %>% select(!state)
  missing_br_upd <- left_join(missing_br, india_br, by = c("year"))
  df <- df %>% filter(!is.na(br)) %>% bind_rows(missing_br_upd) %>% arrange(id, year)
  
  # Calculate births and pre-term births per year
  df <- df %>% mutate(
    births_yearly = population * br / 1000,
    births_daily = births_yearly / 365.25,
    per_preterm = 0.130 - (0.001 * (year - 2020)),
    per_preterm_lb = 0.097 - (0.002 * (year - 2020)),
    per_preterm_ub = 0.173 + (0.001 * (year - 2020)),
    births_preterm_daily = births_daily * per_preterm,
    births_preterm_yearly = births_yearly * per_preterm,
    births_preterm_daily_lb = births_daily * per_preterm_lb,
    births_preterm_daily_ub = births_daily * per_preterm_ub,
  )

#### TEMPERATURE and TEMP-HEALTH RISK RATIO #### 
  
  # Import means by urban area
  means <- readRDS("02 intermediate/07 hist and proj temp by urban area data.r") %>%
    select(id, hist_15_24) %>%
    rename(mean = hist_15_24) %>%
    mutate(
      mean = round(mean, 0), # Round to nearest integer
      mean = case_when(
        is.na(mean) ~ mean[id == 906], # Fill missing with nearest urban area
        TRUE ~ mean)
      )

  # Import temperature
  temp <- readRDS("02 intermediate/06 temperature data.r") %>%
    mutate(year = as.numeric(year))
  
  # Merge by id
  temp_upd <- left_join(means, temp, by = c("id")) %>%
    mutate(
      incr_th = 1.04,
      incr_th_lb = 1.03,
      incr_th_ub = 1.06,
      th = incr_th^(mean_temp_lb-mean), # Get RR based distance from mean temp
      th_lb = incr_th_lb^(mean_temp_lb-mean),
      th_ub = incr_th_ub^(mean_temp_lb-mean),
    ) %>% relocate(id, year)
  
  # Merge into analysis file
  df <- left_join(df, temp_upd, by = c("id", "year"))

#### OTHER RISK RATIOS ####
  
  # Add intervention effectiveness
  df <- df %>% mutate(
    ih = 1.18,
    ih_lb = 1.00, # 1.07,
    ih_ub = 1.30
  )
  
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
    br = "Annual births per 1,000 people",
    births_yearly = "Annual births",
    births_daily = "Daily births",
    per_preterm = "Percentage of births that are preterm",
    per_preterm_lb = "Percentage of births that are preterm (lower bound)",
    per_preterm_ub = "Percentage of births that are preterm (upper bound)",
    births_preterm_daily = "Daily preterm births",
    births_preterm_daily_lb = "Daily preterm births (lower bound)",
    births_preterm_daily_ub = "Daily preterm births (upper bound)",
    mean = "Observed mean temperature of the urban area from 2015-2024 rounded to the nearest integer",
    mean_temp_lb = "Lower bound of mean temperature interval for heat-preterm birth exposure-response ratios",
    mean_temp_ub = "Upper bound of mean temperature interval for heat-preterm birth exposure-response ratios",
    ssp245 = "The number of days within the mean and max temperature intervals for the given year",
    th = "The exposure-response ratio of heat on preterm births",
    th_lb = "The exposure-response ratio of heat on preterm births (lower bound)",
    th_ub = "The exposure-response ratio of heat on preterm births (upper bound)",
    ih = "The change in Atosiban distribution effectiveness based on temperature",
    ih_lb = "The change in Atosiban distribution effectiveness based on temperature (lower bound)",
    ih_ub = "The change in Atosiban distribution effectiveness based on temperature (upper bound)",
    cs_main = "The percent improvement in effectiveness of Atosiban distribution when complimented by a HHWS",
    cs_low = "The percent improvement in effectiveness of Atosiban distribution when complimented by a HHWS (lower bound)",
    cs_high = "The percent improvement in effectiveness of Atosiban distribution when complimented by a HHWS (upper bound)",
  )
  
  # Save
  saveRDS(df, file = "02 intermediate/20 analysis file.r")
