
# Code description --------------------------------------------------------

# This code updates the Evans Spring flow and temperature data through July 2021.


# Libraries ---------------------------------------------------------------

library(tidyverse) #this package is used for basic data wrangling and plotting
library(lubridate) #this package is used to convert date formats and make them easier to work with
library(lfstat) #this package has the code to automatically calculate water year from a date
library(plotly) #this package enables us to make interactive plots
library(accelerometry)


# Load data ---------------------------------------------------------------

Evans_data_2020 <- read_csv("data/EVN_raw/EVN_data_ALL.csv")
Evans_data_Oct_2020_Jan_2021 <- read_csv("data/EVN_raw/EVN_20201014_20210121.csv")
Evans_data_Feb_2021_Jul_2021 <- read_csv("data/EVN_raw/EVN_20210202_20210726.csv")


# Wrangle data ------------------------------------------------------------

#Format dates and times
Evans_data_2020$Date <- as.Date(Evans_data_2020$Date, "%m/%d/%Y")
Evans_data_2020$DateTime2 <- mdy_hm(Evans_data_2020$DateTime)
Evans_data_2020  <- Evans_data_2020 %>% 
  rename(flow_cfs = `Calc Flow (cfs)`)
Evans_data_2020$water_year <- water_year(Evans_data_2020$Date, origin = "usgs")

Evans_data_Oct_2020_Jan_2021$Date <- as.Date(Evans_data_Oct_2020_Jan_2021$Date, 
                                            "%m/%d/%Y")
Evans_data_Oct_2020_Jan_2021$DateTime <- with(Evans_data_Oct_2020_Jan_2021, ymd(Date) + hms(Time))
Evans_data_Oct_2020_Jan_2021$water_year <- water_year(Evans_data_Oct_2020_Jan_2021$Date, origin = "usgs")


Evans_data_Feb_2021_Jul_2021$Date <- as.Date(Evans_data_Feb_2021_Jul_2021$Date, 
                                             "%m/%d/%Y")
Evans_data_Feb_2021_Jul_2021$DateTime <- with(Evans_data_Feb_2021_Jul_2021, ymd(Date) + hms(Time))
Evans_data_Feb_2021_Jul_2021$water_year <- water_year(Evans_data_Feb_2021_Jul_2021$Date, origin = "usgs")

#combine date_time, water_year, stage, flow, temp into single, main dataframe

#look up EVN rating curve, calculate flow in working dataframes, then select and rename columns and rbind into main dataframe.

Evans_data_2020_clean <- Evans_data_2020 %>% 
  select(DateTime2, LEVEL, TEMPERATURE, water_year) %>% 
  filter(DateTime2 < ymd_hms("2020-10-14 13:30:00")) %>% 
  rename(date_time = DateTime2, stage_m = LEVEL, temp_C = TEMPERATURE)

Evans_data_Oct_2020_Jan_2021_clean <- Evans_data_Oct_2020_Jan_2021 %>% 
  select(DateTime, LEVEL, TEMPERATURE, water_year) %>% 
  rename(date_time = DateTime, stage_m = LEVEL, temp_C = TEMPERATURE)

Evans_data_all <- rbind(Evans_data_2020_clean, Evans_data_Oct_2020_Jan_2021_clean)

Evans_data_Feb_2021_Jul_2021_clean <- Evans_data_Feb_2021_Jul_2021 %>% 
  select(DateTime, LEVEL, TEMPERATURE, water_year) %>% 
  rename(date_time = DateTime, stage_m = LEVEL, temp_C = TEMPERATURE)

Evans_data_all <- rbind(Evans_data_all, Evans_data_Feb_2021_Jul_2021_clean)

Evans_data_all$flow_cfs <- 185.32*Evans_data_all$stage_m^2.2756

#plot and review



ggplotly(
  ggplot(data = Evans_data_all) +
    geom_line(aes(x = date_time, y = flow_cfs), color = "midnightblue")
)

#last point looks bad - remove

Evans_data_all_QA <- Evans_data_all %>% 
  filter(date_time < ymd_hms("2021-07-26 14:45:00"))

ggplotly(
  ggplot(data = Evans_data_all_QA) +
    geom_line(aes(x = date_time, y = stage_m), color = "midnightblue")
)

ggplotly(
  ggplot(data = Evans_data_all_QA) +
    geom_line(aes(x = date_time, y = flow_cfs), color = "midnightblue")
)

ggplotly(
  ggplot(data = Evans_data_all_QA) +
    geom_line(aes(x = date_time, y = temp_C), color = "mediumvioletred")
)


# Export and save full file

write_csv(Evans_data_all_QA, "data/EVN_processed/EVN_all_flow_temp.csv")
