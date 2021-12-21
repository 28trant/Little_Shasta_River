
# Code description --------------------------------------------------------

# This code was developed to analyze the flow and temperature data from LSR and LSM for water years 2018 through 2021.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(lfstat)
library(plotly)


# Load data ---------------------------------------------------------------

LSR_flow_temp_wy2018_2021 <- read_csv("data/LSR_processed/LSR_stage_flow_wy2017_2021.csv")
LSM_stage_flow_temp_all <- readRDS("data/LSM_processed/LSM_stage_flow_temp_all.rds")


# Wrangle data ------------------------------------------------------------
LSR_flow_temp_wy2018_2021 <- LSR_flow_temp_wy2018_2021 %>% 
  rename(date_time = Date)

LSR_flow_temp_wy2018_2021$date_time <- mdy_hm(LSR_flow_temp_wy2018_2021$date_time)

LSR_flow_temp_wy2018_2021$water_year <- water_year(LSR_flow_temp_wy2018_2021$date_time, origin = "usgs")
LSR_flow_temp_wy2018_2021$water_year <- as.numeric(as.character(LSR_flow_temp_wy2018_2021$water_year))

LSM_wy2018_2021 <- LSM_stage_flow_temp_all %>% 
  filter(water_year > 2017) %>% 
  filter(water_year < 2022)


# Plot and review ---------------------------------------------------------

ggplotly(
  ggplot() +
    geom_line(data = LSR_flow_temp_wy2018_2021, aes(x=date_time, y=LSR_flow_cfs), color = "midnightblue") +
    geom_line(data = LSM_wy2018_2021, aes(x=date_time, y=flow_cfs), color = "mediumvioletred")
)
LSR_and_LSM <- left_join(LSR_flow_temp_wy2018_2021, LSM_wy2018_2021, by = "date_time")

LSR_and_LSM_filtered <- LSR_and_LSM %>% 
  select(date_time, LSR_flow_cfs, flow_cfs, water_year.x) %>% 
  rename(LSM_flow_cfs = flow_cfs, water_year = water_year.x) %>% 
  filter(!is.na(LSM_flow_cfs))

LSR_and_LSM_filtered$delta_flow <- LSR_and_LSM_filtered$LSR_flow_cfs - LSR_and_LSM_filtered$LSM_flow_cfs

summary(LSR_and_LSM_filtered$delta_flow)

ggplotly(
  ggplot(data = LSR_and_LSM_filtered) +
    geom_line(aes(x = date_time, y = LSR_flow_cfs), color = "midnightblue") +
    geom_line(aes(x = date_time, y = LSM_flow_cfs), color = "mediumvioletred")
)

#filter flows above 150cfs

LSR_and_LSM_filtered_flows <- LSR_and_LSM_filtered %>% 
  filter(LSR_flow_cfs < 150) %>% 
  filter(LSM_flow_cfs < 150)

ggplotly(
  ggplot(data = LSR_and_LSM_filtered_flows) +
    geom_line(aes(x = date_time, y = LSR_flow_cfs), color = "midnightblue") +
    geom_line(aes(x = date_time, y = LSM_flow_cfs), color = "mediumvioletred")
)

summary(LSR_and_LSM_filtered_flows$delta_flow)
