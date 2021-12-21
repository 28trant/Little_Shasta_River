
# Code description --------------------------------------------------------

# This code was developed to review the stage, flow, and temperature data from the Little Shasta River upstream of the Musgrave diversion (site id: LSM). Data extends from July 2017 through early December 2021.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(lfstat)
library(plotly)


# Load data ---------------------------------------------------------------

LSM_stage_flow_temp_all <- read_csv("data/LSM_raw/LSM_stage_flow_temp_ALL.csv")


# Wrangle data ------------------------------------------------------------

#formal date_time column
LSM_stage_flow_temp_all$date_time <- mdy_hm(LSM_stage_flow_temp_all$date_time)

LSM_date_time_NA <- LSM_stage_flow_temp_all %>% 
  filter(is.na(temp_C))
#all the NAs correspond to periods when we took spot measurements

#add water year
LSM_stage_flow_temp_all$water_year <- water_year(LSM_stage_flow_temp_all$date_time, origin = "usgs")
LSM_stage_flow_temp_all$water_year <- as.numeric(as.character(LSM_stage_flow_temp_all$water_year))

fill_na(LSM_stage_flow_temp_all$stage_m, max.len = 2)

LSM_stage_flow_temp_all$stage_m <- fill_na(LSM_stage_flow_temp_all$stage_m, max.len = 2)
LSM_stage_flow_temp_all$flow_cfs <- fill_na(LSM_stage_flow_temp_all$flow_cfs, max.len = 2)
LSM_stage_flow_temp_all$temp_C <- fill_na(LSM_stage_flow_temp_all$temp_C, max.len = 2)


# Plot and review ---------------------------------------------------------

ggplotly(
  ggplot(data = LSM_stage_flow_temp_all) +
    geom_line(aes(x = date_time, y = stage_m), color = "midnightblue")
)

LSM_stage_flow_temp_all_filtered <- LSM_stage_flow_temp_all %>% 
  filter(stage_m > 0.003)

ggplotly(
  ggplot(data = LSM_stage_flow_temp_all_filtered) +
    geom_line(aes(x = date_time, y = stage_m), color = "midnightblue")
)

ggplotly(
  ggplot(data = LSM_stage_flow_temp_all_filtered) +
    geom_line(aes(x = date_time, y = flow_cfs), color = "midnightblue")
)

ggplotly(
  ggplot(data = LSM_stage_flow_temp_all_filtered) +
    geom_line(aes(x = date_time, y = temp_C), color = "mediumvioletred")
)

#this all looks good. Save as rds file


# Save --------------------------------------------------------------------

write_rds(LSM_stage_flow_temp_all_filtered, "data/LSM_processed/LSM_stage_flow_temp_all.rds", compress = "gz")
