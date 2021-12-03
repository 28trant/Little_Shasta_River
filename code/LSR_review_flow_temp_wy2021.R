
# Code description --------------------------------------------------------

# The purpose of this code is to update the flow and temperature files for site LSR to include all data through the end of water year 2021 (ending September 30, 2021). Data will be reformatted as an rds file and the csv will be removed to save space on the repository.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(lfstat)
library(plotly)


# Load data ---------------------------------------------------------------

LSR_flow_all <- read_csv("data/LSR_processed/LSR_flow_all.csv")
LSR_temp_all <- read_csv("data/LSR_processed/LSR_temp_2017_2020.csv")
LSR_flow_and_temp_2021 <- read_csv("data/LSR_raw/LSR_flow_temp_2017_2021.csv")

# Plot and review existing data -------------------------------------------

#Plot existing flow and temp data to see where the datasets end and the new data should be used to supplement

ggplotly(
  ggplot(data = LSR_flow_all) +
    geom_line(aes(x = date, y = daily_mean_cfs), color= "midnightblue")
)

#Daily mean flow goes through 4/22/2021. Need to calculate daily mean flow from new dataset and update.

ggplotly(
  ggplot(data = LSR_temp_all) +
    geom_line(aes(x = date_time2, y = temp_C), color = "mediumvioletred")
)

#temperature data only extends through water year 2020 (September 30, 2020). Need to add the complete water year for 2021.

#reformat Date as date_time
LSR_flow_and_temp_2021 <- LSR_flow_and_temp_2021 %>% 
  mutate(date_time = mdy_hm(Date))


# Wrangle new data --------------------------------------------------------

#Clean NAs out of the flow data, QA, calculate daily mean flow, add to LSR_all

LSR_flow_2021 <- LSR_flow_and_temp_2021 %>% 
  select(date_time, calc_flow_cfs) %>% 
  filter(!is.na(calc_flow_cfs))

#plot and review
ggplotly(
  ggplot(data = LSR_flow_2021) + 
    geom_line(aes(x = date_time, y = calc_flow_cfs), color = "midnightblue")
)

min(LSR_flow_2021$calc_flow_cfs)

#Flows are not calculated for the lowest flows. Download stage data for water year 2021 and recalculate flows based on new rating curve.