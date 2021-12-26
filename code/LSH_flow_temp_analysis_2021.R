
# Code description --------------------------------------------------------

# This code was developed to look at the flow and temperature patterns at LSH. Flow is calculated from stage collected at the real-time station. The rating curve is defined in this code. The period of record extends from July 1, 2021 through September 30, 2021.


# Library -----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(lfstat)
library(accelerometry)
library(plotly)


# Load data ---------------------------------------------------------------

LSH_stage_temp_2020_2021 <- read_csv("data/LSH_raw/LSH_flow_temp_2020_2021.csv")


# Wrangle data ------------------------------------------------------------

#format date and time
LSH_stage_temp_2020_2021$date_time <- mdy_hm(LSH_stage_temp_2020_2021$date_time)

#add water year
LSH_stage_temp_2020_2021$water_year <- water_year(LSH_stage_temp_2020_2021$date_time, origin = "usgs")
LSH_stage_temp_2020_2021$water_year <- as.numeric(as.character(LSH_stage_temp_2020_2021$water_year))


# Flow: Plot and review ---------------------------------------------------------

ggplotly(
  ggplot(data = LSH_stage_temp_2020_2021) +
    geom_line(aes(x = date_time, y  = calc_flow_cfs), color = "midnightblue")
)

#The current rating curve only applies to data beginning July 1, 2021. Filter and review.

LSH_flow_2021 <- LSH_stage_temp_2020_2021 %>% 
  select(date_time, calc_flow_cfs) %>% 
  filter(date_time >= ymd_hms("2021-07-01 00:00:00"))

ggplotly(
  ggplot(data = LSH_flow_2021) +
    geom_line(aes(x = date_time, y  = calc_flow_cfs), color = "midnightblue")
)

mean_baseflow_2021 <- mean(LSH_flow_2021$calc_flow_cfs)


# Temp: Plot and review ---------------------------------------------------

ggplotly(
  ggplot(data = LSH_stage_temp_2020_2021) +
    geom_line(aes(x = date_time, y  = temp_C), color = "mediumvioletred")
)

# Temperatures are way too moderated starting on Feb 14, 2021. Possibly the sensor is buried. Need to find and clean the sensor. Average temperatures are illustrated, but not the diurnal range.
