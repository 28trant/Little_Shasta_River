
# Code description --------------------------------------------------------

# This code was developed to analyze the hydrologic year type of runoff at the LSR gage on the Little Shasta River. Water years include 1951-2021. Estimated unimpaired runoff was downloaded from rivers.codefornature.org on 12/20/2021. Estimated unimpaired runoff is calculated as the monthly average flow. Hydrologic year types are calculated using tercile groupings to be consistent with the methods used in functional flow analyses.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(lfstat)
library(plotly)


# Load data ---------------------------------------------------------------

LSR_monthly_flow_1950_2021 <- read_csv("data/LSR_processed/flow_3917176_mean_estimated_1950_2021.csv")


# Wrangle data ------------------------------------------------------------

#make a date-formatted column
LSR_monthly_flow_1950_2021 <- LSR_monthly_flow_1950_2021 %>% 
  mutate(year_month = make_date(year, month))

#add the water year
LSR_monthly_flow_1950_2021$water_year <- water_year(LSR_monthly_flow_1950_2021$year_month, origin = "usgs")
LSR_monthly_flow_1950_2021$water_year <- as.numeric(as.character(LSR_monthly_flow_1950_2021$water_year))

#filter to include only whole water years (wy1951-2021)
LSR_monthly_flow_1951_2021 <- LSR_monthly_flow_1950_2021 %>% 
  filter(water_year > 1950)

#calculate total annual flow from monthly estimated average flows
LSR_annual_flow_1951_2021 <- LSR_monthly_flow_1951_2021 %>% 
  group_by(water_year) %>% 
  summarize(TAF = sum(value))


# Determine water year types ----------------------------------------------

#Find 33rd percentile

LSR_percentile_33 <- quantile(LSR_annual_flow_1951_2021$TAF, 0.33)

LSR_percentile_66 <- quantile(LSR_annual_flow_1951_2021$TAF, 0.667)

LSR_3_WYT <- LSR_annual_flow_1951_2021 %>% 
  mutate(WYT = ifelse(TAF >= LSR_percentile_66, "wet",
                      ifelse(TAF < LSR_percentile_66 & TAF >
                               LSR_percentile_33, "moderate", "dry")))

write_csv(LSR_3_WYT, file = "output/LSR_3_WYT.csv")
