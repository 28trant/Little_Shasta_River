
# Code description --------------------------------------------------------

# This code was developed to review the stage, flow, and temperature data from the Little Shasta River upstream of the Musgrave diversion (site id: LSM).


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(lfstat)
library(plotly)


# Load data ---------------------------------------------------------------

LSM_stage_flow_temp_all <- read_csv("data/LSM_raw/LSM_stage_flow_temp_ALL.csv")


# Wrangle data ------------------------------------------------------------

#rename columns
LSM_stage_flow_temp_all_filtered <- LSM_stage_flow_temp_all %>% 
  select(Date, Time, DateTime, "LS.Stage..m.", "Flow (cfs)", TEMPERATURE, "Measured Flow (cfs)") %>% 
  rename(date_time = DateTime, stage_m = "LS.Stage..m.", flow_cfs = "Flow (cfs)", temp_C = TEMPERATURE, spot_flow_cfs = "Measured Flow (cfs)")

#formal date_time column
LSM_stage_flow_temp_all_filtered$date_time <- mdy_hm(LSM_stage_flow_temp_all_filtered$date_time)

LSM_date_time_NA <- LSM_stage_flow_temp_all_filtered %>% 
  filter(is.na(date_time))

#add water year
LSM_stage_flow_temp_all_filtered$water_year <- water_year(LSM_stage_flow_temp_all_filtered$date_time, origin = "usgs")
