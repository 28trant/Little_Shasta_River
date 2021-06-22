
# Code description --------------------------------------------------------

# This code is used to wrangle and QA the temperature data for the Hart Ranch Management Plan (time series through the 2020 water year).


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(plotly)
library(lubridate)
library(lfstat)



# Load data ---------------------------------------------------------------

LSR_data <- read_csv("data/LSR_raw/LSR_temp_2017_2021.csv")


# Wrangle data ------------------------------------------------------------

#Rename columns
LSR_temp <- LSR_data %>% 
  select(Date, `TNC LS Host / WTemp - degrees C`) %>% 
  rename(date_time = "Date", temp_C = `TNC LS Host / WTemp - degrees C`) %>% 
  filter(!is.na(temp_C))

#Format dates
LSR_temp$date_time2 <- mdy_hm(LSR_temp$date_time, tz=Sys.timezone())
LSR_temp$date <- as.Date(LSR_temp$date_time2)

#Filter out 2021 water year
LSR_temp_2020 <- LSR_temp %>% 
  filter(date < "2020-10-01")

#Add water year
LSR_temp_2020$water_year <- water_year(LSR_temp_2020$date, origin = "usgs")

# Plot and review ---------------------------------------------------------

ggplot(data = LSR_temp_2020) +
  geom_line(aes(x=date_time2, y=temp_C), color = "maroon4") +
  labs(x = "date", y=expression("water temperature " (degree*C))) +
  theme_bw()

#Looks good! Clean up file and save out data

LSR_temp_2020_QA <- LSR_temp_2020 %>% 
  select(date_time2, date, temp_C, water_year)
  
write_csv(LSR_temp_2020_QA, file = "data/LSR_processed/LSR_temp_2017_2020.csv")

