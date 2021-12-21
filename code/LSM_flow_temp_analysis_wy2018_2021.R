
# Code description --------------------------------------------------------

# This code was developed to analyze the flow and temperature data from LSR and LSM for water years 2018 through 2021.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(plotly)


# Load data ---------------------------------------------------------------

LSM_stage_flow_temp_all <- readRDS("data/LSM_processed/LSM_stage_flow_temp_all.rds")


# Wrangle data ------------------------------------------------------------

LSM_wy2018_2021 <- LSM_stage_flow_temp_all %>% 
  filter(water_year > 2017) %>% 
  filter(water_year < 2022)

write_csv(LSM_wy2018_2021, "data/LSM_processed/LSM_stage_flow_temp_wy2018_2021.csv")



