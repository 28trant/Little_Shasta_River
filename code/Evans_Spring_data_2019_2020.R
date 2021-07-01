
# Code description --------------------------------------------------------

# This code was developed to review and plot the flow and temperature data for Evans Spring for the Hart Ranch Management Plan 5-year update.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(lfstat)
library(plotly)
library(accelerometry)


# Load data ---------------------------------------------------------------

Evans_data <- read_csv("data/EVN_raw/EVN_data_ALL.csv")
Evans_WQ <- read_csv("data/EVN_raw/EVN_WQ_raw.csv")

# Wrangle data ------------------------------------------------------------

Evans_data$Date <- as.Date(Evans_data$Date, "%m/%d/%Y")
Evans_WQ$Date <- as.Date(Evans_WQ$Date, "%m/%d/%Y")

Evans_WQ$Date2 <- as.character(Evans_WQ$Date)
Evans_WQ$Date3 <- ymd(Evans_WQ$Date2)

Evans_data$DateTime2 <- mdy_hm(Evans_data$DateTime)

Evans_data  <- Evans_data %>% 
  rename(flow_cfs = `Calc Flow (cfs)`)

Evans_data$water_year <- water_year(Evans_data$Date, origin = "usgs")

# Plot and review ---------------------------------------------------------

#Flow
ggplotly(
  ggplot(data = Evans_data) +
    geom_line(aes(x=DateTime2, y=flow_cfs), color = "midnightblue")
)

#looks good! Format and save out.

ggplot(data = Evans_data) +
  geom_line(aes(x=DateTime2, y=flow_cfs), color = "midnightblue") +
  scale_x_datetime(date_breaks = "2 month") +
  labs(x = " ", y = expression(paste("flow (ft"^"3","/s)"))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

ggsave("Evans_Spring_discharge.png", path="output/", width=6, height=3.5, units="in", dpi=300)

#Temperature
ggplotly(
  ggplot(data = Evans_data) +
    geom_line(aes(x=DateTime2, y=TEMPERATURE), color = "maroon4")
)

ggplot(data = Evans_data) +
  geom_line(aes(x=DateTime2, y=TEMPERATURE), color = "maroon4") +
  scale_x_datetime(date_breaks = "2 month") +
  ylim(4,14) +
  labs(x = " ", y = expression(paste("water temperature ("^"o","C)"))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

ggsave("Evans_Spring_temperature.png", path="output/", width=6, height=3.5, units="in", dpi=300)

#Water Quality
Evans_TN <- Evans_WQ %>% 
  select(Date3, TN_ppm)

#Nitrogen
ggplot(data = Evans_TN, aes(x = Date3, y = TN_ppm)) +
  geom_bar(stat = "identity", position = "dodge") #+
  # geom_hline(yintercept = 0.1, color = "deeppink4") +
  # labs(x = "date", y = "TN (mg/L)") +
  # scale_fill_manual("ID", values = c("EVN" = "midnight blue")) +
  # theme_classic()

base_TN
