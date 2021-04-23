
# Code description --------------------------------------------------------

# This code is used to analyze data for the Hart Ranch Management Plan Update. As it is a work in progress, it may develop into a set of codes that might get saved in a Hart_RMP_code folder. We'll use github for version control.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(plotly)
library(lubridate)
library(lfstat)

# Load data ---------------------------------------------------------------

LSR_data <- read_csv("data/LSR_raw/LSR_discharge_2017_2021.csv")

LSR_data_2016_2017 <- read_csv("data/LSR_raw/LSR_flow_2016_2017.csv")

LSH_data <- read_csv("data/LSH_raw/LSH_discharge_2017_2021.csv")


# Wrangle data ------------------------------------------------------------

LSR_flow <- LSR_data %>% 
  select(DateTime, `Flow (cfs)`) %>% 
  rename(date_time = "DateTime", flow_cfs = `Flow (cfs)`) %>% 
  filter(!is.na(flow_cfs))

LSR_flow_2016_2017 <- LSR_data_2016_2017 %>% 
  select(date_time, flow_cfs) %>% 
  filter(!is.na(flow_cfs))

#format dates

LSR_flow$date_time <- mdy_hm(LSR_flow$date_time) #turns the object into a POSIXct, not Date. Check
LSR_flow$date <- as.Date(LSR_flow$date_time) #makes a new column for the date (but not time)

LSR_flow_2016_2017$date_time <- mdy_hm(LSR_flow_2016_2017$date_time)
LSR_flow_2016_2017$date <- as.Date(LSR_flow_2016_2017$date_time)


#add columns to define water year and water year day, calculate daily average flow

LSR_flow_WY <- LSR_flow %>% 
  mutate(Julian_Day=yday(date), water_year = water_year(date, "usgs")) %>% 
  group_by(date, Julian_Day, water_year) %>% 
  summarize(daily_mean_cfs = mean(flow_cfs))

LSR_flow_WY_2016_2017 <- LSR_flow_2016_2017 %>% 
  mutate(Julian_Day=yday(date), water_year = water_year(date, "usgs")) %>% 
  group_by(date, Julian_Day, water_year) %>% 
  summarize(daily_mean_cfs = mean(flow_cfs))
# Plot and review ---------------------------------------------------------

ggplotly(
  ggplot() + geom_line(data = LSR_flow_WY, aes(x=date, y=daily_mean_cfs), color = "royalblue3") +
    labs(x = "date", y = "discharge (cfs)") +
    theme_bw()
)

ggplotly(
  ggplot() + geom_line(data = LSR_flow_WY_2016_2017, aes(x=date, y=daily_mean_cfs), color = "royalblue3") +
    labs(x = "date", y = "discharge (cfs)") +
    theme_bw()
)

#Data looks great! Save out as the processed (QA'd) file

write_csv(LSR_flow_WY, "data/LSR_processed/LSR_flow_2017_2020.csv")

write_csv(LSR_flow_WY_2016_2017, "data/LSR_processed/LSR_flow_2016_2017.csv")

#bind into one master file

LSR_flow_all <- rbind(LSR_flow_WY_2016_2017, LSR_flow_WY)

ggplotly(
  ggplot() + geom_line(data = LSR_flow_all, aes(x=date, y=daily_mean_cfs), color = "royalblue3") +
    labs(x = "date", y = "discharge (cfs)") +
    theme_bw()
)

#SAVE!!!

write_csv(LSR_flow_all, "data/LSR_processed/LSR_flow_all.csv")

# Repeat for LSH ----------------------------------------------------------


LSH_flow <- LSH_data %>% 
  select(DateTime, `CalcFlow (cfs)`) %>% 
  rename(date_time = "DateTime", flow_cfs = `CalcFlow (cfs)`) %>% 
  filter(!is.na(flow_cfs))

#format dates

LSH_flow$date_time <- mdy_hm(LSH_flow$date_time) #turns the object into a POSIXct, not Date. Check
LSH_flow$date <- as.Date(LSH_flow$date_time) #makes a new column for the date (but not time)

#add columns to define water year and water year day, calculate daily average flow

LSH_flow_WY <- LSH_flow %>% 
  mutate(Julian_Day=yday(date), water_year = water_year(date, "usgs")) %>% 
  group_by(date, Julian_Day, water_year) %>% 
  summarize(daily_mean_cfs = mean(flow_cfs))


# Plot and review ---------------------------------------------------------

ggplotly(
  ggplot() + geom_line(data = LSH_flow_WY, aes(x=date, y=daily_mean_cfs), color = "royalblue3") +
    #geom_line(data = LSR_flow_WY, aes(x=date, y=daily_mean_cfs), color = "magenta") +
    labs(x = "date", y = "discharge (cfs)") +
    theme_bw()
)

#Data looks almost exactly like LSR, which doesn't make sense because of the Musgrave diversion. Also, the data records overlap for almost the whole time, and that doesn't make sense because of the monitoring we did - no stage, just temp until the new station went in. Need to check this with Amber before saving out. 



