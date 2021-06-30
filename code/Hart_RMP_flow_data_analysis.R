
# Code description --------------------------------------------------------

# This code is developed to analyze the stream data for the Hart Ranch Management Plan update. It may included all data, but was initially developed to explore stream flow data.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(plotly)
library(accelerometry)


# Load data ---------------------------------------------------------------

LSR_flow <- read_csv("data/LSR_processed/LSR_flow_all.csv")
LSR_temp <- read_csv("data/LSR_processed/LSR_temp_2017_2020.csv")

# Flow data ------------------------------------------------------------

LSR_flow$month <- factor(strftime(LSR_flow$date, "%b"), levels = month.abb)

#Box plot of flow by month
ggplot(data = LSR_flow, aes(x=,month, y=daily_mean_cfs)) +
  geom_boxplot() +
  labs(x = " ", y = expression(paste("mean daily stream flow (ft"^"3","/s)"))) +
  theme_bw()

ggsave("LSR_discharge_mean_daily_by_month.png", path="output/", width=6, height=3.5, units="in", dpi=300)

#Mean monthly flow

mean_monthly_flow <- LSR_flow %>% 
  group_by(month) %>% 
  summarize(mean_flow_cfs = mean(daily_mean_cfs))


# Temp data ---------------------------------------------------------------

LSR_temp$month <- factor(strftime(LSR_temp$date, "%b"), levels = month.abb)

#Make daily max data frame
LSR_daily_max_temp <- LSR_temp %>% 
  select(date, month, water_year, temp_C) %>% 
  group_by(date,month, water_year) %>% 
  summarize(daily_max_C = max(temp_C))

#Make daily average data frame
LSR_daily_mean_temp <- LSR_temp %>% 
  select(date, month, water_year, temp_C) %>% 
  group_by(date,month, water_year) %>% 
  summarize(daily_mean_C = mean(temp_C))

#Explore lengths of consecutive days, annual maximums, MWMT, and MWAT by water year

#Annual max
annual_max_all <- LSR_daily_max_temp %>%
  group_by(water_year) %>% 
  arrange(desc(daily_max_C)) %>% 
  slice(1)

#2017
LSR_daily_max_temp_2017 <- LSR_daily_max_temp %>% 
  filter(water_year == 2017)

#Days when temp >20C
Days_abv_20C_2017 <- LSR_daily_max_temp_2017 %>% 
  filter(daily_max_C >= 20)

#MWMT
LSR_2017_7DMax <- movingaves(LSR_daily_max_temp_2017$daily_max_C,7)
LSR_2017_7DMax <- data.frame(LSR_2017_7DMax)
MWMT_2017 <- max(LSR_2017_7DMax)

#get dates of MWMT
start_MWMT_2017 <- which(LSR_2017_7DMax$LSR_2017_7DMax==MWMT_2017)
end_MWMT_2017 <- start_MWMT_2017+6

LSR_daily_max_temp_2017[start_MWMT_2017,]
LSR_daily_max_temp_2017[end_MWMT_2017,]

#MWAT

LSR_daily_mean_temp_2017 <- LSR_daily_mean_temp %>% 
  filter(water_year == 2017)

LSR_2017_7DMean <- movingaves(LSR_daily_mean_temp_2017$daily_mean_C,7)
LSR_2017_7DMean <- data.frame(LSR_2017_7DMean)

MWAT_2017 <- max(LSR_2017_7DMean$LSR_2017_7DMean)

#get dates of MWAT
start_MWAT_2017 <- which(LSR_2017_7DMean$LSR_2017_7DMean==MWAT_2017)
end_MWAT_2017 <- start_MWAT_2017+6

LSR_daily_max_temp_2017[start_MWAT_2017,]
LSR_daily_max_temp_2017[end_MWAT_2017,]

#2018
LSR_daily_max_temp_2018 <- LSR_daily_max_temp %>% 
  filter(water_year == 2018)

#Days when temp >20C
Days_abv_20C_2018 <- LSR_daily_max_temp_2018 %>% 
  filter(daily_max_C >= 20)

#Annual max
annual_max_2018 <- LSR_daily_max_temp_2018 %>% 
  slice_max(daily_max_C) #I don't know why this isn't working

#MWMT
LSR_2018_7DMax <- movingaves(LSR_daily_max_temp_2018$daily_max_C,7)
LSR_2018_7DMax <- data.frame(LSR_2018_7DMax)
MWMT_2018 <- max(LSR_2018_7DMax)

#get dates of MWMT
start_MWMT_2018 <- which(LSR_2018_7DMax$LSR_2018_7DMax==MWMT_2018)
end_MWMT_2018 <- start_MWMT_2018+6

LSR_daily_max_temp_2018[start_MWMT_2018,]
LSR_daily_max_temp_2018[end_MWMT_2018,]

#MWAT

LSR_daily_mean_temp_2018 <- LSR_daily_mean_temp %>% 
  filter(water_year == 2018)

LSR_2018_7DMean <- movingaves(LSR_daily_mean_temp_2018$daily_mean_C,7)
LSR_2018_7DMean <- data.frame(LSR_2018_7DMean)

MWAT_2018 <- max(LSR_2018_7DMean$LSR_2018_7DMean)

#get dates of MWAT
start_MWAT_2018 <- which(LSR_2018_7DMean$LSR_2018_7DMean==MWAT_2018)
end_MWAT_2018 <- start_MWAT_2018+6

LSR_daily_max_temp_2018[start_MWAT_2018,]
LSR_daily_max_temp_2018[end_MWAT_2018,]

#2019
LSR_daily_max_temp_2019 <- LSR_daily_max_temp %>% 
  filter(water_year == 2019)

#Days when temp >20C
Days_abv_20C_2019 <- LSR_daily_max_temp_2019 %>% 
  filter(daily_max_C >= 20)

#Annual max
annual_max_2019 <- LSR_daily_max_temp_2019 %>% 
  slice_max(daily_max_C) #I don't know why this isn't working

#MWMT
LSR_2019_7DMax <- movingaves(LSR_daily_max_temp_2019$daily_max_C,7)
LSR_2019_7DMax <- data.frame(LSR_2019_7DMax)
MWMT_2019 <- max(LSR_2019_7DMax)

#get dates of MWMT
start_MWMT_2019 <- which(LSR_2019_7DMax$LSR_2019_7DMax==MWMT_2019)
end_MWMT_2019 <- start_MWMT_2019+6

LSR_daily_max_temp_2019[start_MWMT_2019,]
LSR_daily_max_temp_2019[end_MWMT_2019,]

#MWAT

LSR_daily_mean_temp_2019 <- LSR_daily_mean_temp %>% 
  filter(water_year == 2019)

LSR_2019_7DMean <- movingaves(LSR_daily_mean_temp_2019$daily_mean_C,7)
LSR_2019_7DMean <- data.frame(LSR_2019_7DMean)

MWAT_2019 <- max(LSR_2019_7DMean$LSR_2019_7DMean)

#get dates of MWAT
start_MWAT_2019 <- which(LSR_2019_7DMean$LSR_2019_7DMean==MWAT_2019)
end_MWAT_2019 <- start_MWAT_2019+6

LSR_daily_max_temp_2019[start_MWAT_2019,]
LSR_daily_max_temp_2019[end_MWAT_2019,]

#2020
LSR_daily_max_temp_2020 <- LSR_daily_max_temp %>% 
  filter(water_year == 2020)

#Days when temp >20C
Days_abv_20C_2020 <- LSR_daily_max_temp_2020 %>% 
  filter(daily_max_C >= 20)

#Annual max
annual_max_2020 <- LSR_daily_max_temp_2020 %>% 
  slice_max(daily_max_C) #I don't know why this isn't working

#MWMT
LSR_2020_7DMax <- movingaves(LSR_daily_max_temp_2020$daily_max_C,7)
LSR_2020_7DMax <- data.frame(LSR_2020_7DMax)
MWMT_2020 <- max(LSR_2020_7DMax)

#get dates of MWMT
start_MWMT_2020 <- which(LSR_2020_7DMax$LSR_2020_7DMax==MWMT_2020)
end_MWMT_2020 <- start_MWMT_2020+6

LSR_daily_max_temp_2020[start_MWMT_2020,]
LSR_daily_max_temp_2020[end_MWMT_2020,]

#MWAT

LSR_daily_mean_temp_2020 <- LSR_daily_mean_temp %>% 
  filter(water_year == 2020)

LSR_2020_7DMean <- movingaves(LSR_daily_mean_temp_2020$daily_mean_C,7)
LSR_2020_7DMean <- data.frame(LSR_2020_7DMean)

MWAT_2020 <- max(LSR_2020_7DMean$LSR_2020_7DMean)

#get dates of MWAT
start_MWAT_2020 <- which(LSR_2020_7DMean$LSR_2020_7DMean==MWAT_2020)
end_MWAT_2020 <- start_MWAT_2020+6

LSR_daily_max_temp_2020[start_MWAT_2020,]
LSR_daily_max_temp_2020[end_MWAT_2020,]
