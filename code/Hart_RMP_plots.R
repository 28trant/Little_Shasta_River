
# Code description --------------------------------------------------------

# This code was developed to plot the discharge, temperature, and water quality figures for the Hart Ranch Management Plan report.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(plotly)
library(lubridate)

# Load data ---------------------------------------------------------------

LSR_flow <- read_csv("data/LSR_processed/LSR_flow_all.csv")

LSR_temp <- read_csv("data/LSR_processed/LSR_temp_2017_2020.csv")

# Wrangle data ------------------------------------------------------------

LSR_flow$date <- ymd(LSR_flow$date)

#Limit data to observations occurring before 2021 water year

LSR_flow_2020 <- LSR_flow %>% 
  filter(water_year< 2021)

# Plots -------------------------------------------------------------------

#LSR discharge
ggplot() + geom_line(data = LSR_flow_2020, aes(x=date, y=daily_mean_cfs), color = "royalblue3") +
  labs(x = "date", y = expression(paste("discharge (ft"^"3","/s)"))) +
  theme_bw()

# Save
ggsave("LSR_discharge_plot_through_WY2020.png", path="output/", width=6, height=3.5, units="in", dpi=300)

#LSR temperature
ggplot(data = LSR_temp) +
  geom_line(aes(x=date_time2, y=temp_C), color = "maroon4") +
  labs(x = "date", y=expression("water temperature " (degree*C))) +
  theme_bw()

#Save
ggsave("LSR_water_temp_plot_through_WY2020.png", path="output/", width=6, height=3.5, units="in", dpi=300)
