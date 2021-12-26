
# Code description --------------------------------------------------------

# This code was developed to calculate temperature metrics for LSR during the 2021 water year. Data is summarized in the AWLSHA2 final report.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(accelerometry)
library(plotly)
library(lubridate)
library(lfstat)


# Load data ---------------------------------------------------------------

LSR_flow_temp_all <- read_csv("data/LSR_raw/LSR_flow_temp_2017_2021.csv")


# Wrangle data ------------------------------------------------------------

# format date_time
LSR_flow_temp_all$date_time <- mdy_hm(LSR_flow_temp_all$Date)

#plot and review
ggplotly(
  ggplot(data = LSR_flow_temp_all) +
    geom_line(aes(x = date_time, y = temp_C), color = "mediumvioletred")
)

LSR_temp_all_QA <- LSR_flow_temp_all %>% 
  filter(temp_C >= 0)

ggplotly(
  ggplot(data = LSR_temp_all_QA) +
    geom_line(aes(x = date_time, y = temp_C), color = "darkorchid3")
)

LSR_temp_all_QA$water_year <- water_year(LSR_temp_all_QA$date_time, origin = "usgs")
LSR_temp_all_QA$water_year <- as.numeric(as.character(LSR_temp_all_QA$water_year))

LSR_temp_all_QA <- LSR_temp_all_QA %>% 
  select(date_time, temp_C, water_year)

#Save temp data as rds file
write_rds(LSR_temp_all_QA, "data/LSR_processed/LSR_temp_2017_2021.rds")


# 2021: Calculate metrics -------------------------------------------------------


LSR_temp_2021 <- LSR_temp_all_QA %>% 
  filter(water_year == "2021")

ggplotly(
  ggplot(data = LSR_temp_2021) +
    geom_line(aes(x = date_time, y = temp_C), color = "darkorchid3")
)

#Make daily max dataframe
LSR_temp_2021$date <- date(LSR_temp_2021$date_time)
LSR_temp_2021$month <- factor(strftime(LSR_temp_2021$date, "%b"), levels = month.abb)

#Make daily max data frame
LSR_daily_max_temp <- LSR_temp_2021 %>% 
  select(date, month, water_year, temp_C) %>% 
  group_by(date,month, water_year) %>% 
  summarize(daily_max_C = max(temp_C))

#Make daily average data frame
LSR_daily_mean_temp <- LSR_temp_2021 %>% 
  select(date, month, water_year, temp_C) %>% 
  group_by(date, month, water_year) %>% 
  summarize(daily_mean_C = mean(temp_C))

#Annual max
max_temp_2021 <- LSR_daily_max_temp %>% 
  arrange(desc(daily_max_C))

#Days when temp >20C
Days_abv_20C_2021 <- LSR_daily_max_temp %>% 
  filter(daily_max_C >= 20)

Days_abv_20C_Jun <- Days_abv_20C_2021 %>% 
  filter(date < ymd("2021-07-01"))

Days_abv_20C_Jul <- Days_abv_20C_2021 %>% 
  filter(date >= ymd("2021-07-01")) %>% 
  filter(date < ymd("2021-08-01"))

#MWMT
LSR_2021_7DMax <- movingaves(LSR_daily_max_temp$daily_max_C,7)
LSR_2021_7DMax <- data.frame(LSR_2021_7DMax)
MWMT_2021 <- max(LSR_2021_7DMax)

#get dates of MWMT
start_MWMT_2021 <- which(LSR_2021_7DMax$LSR_2021_7DMax==MWMT_2021)
end_MWMT_2021 <- start_MWMT_2021+6

LSR_daily_max_temp[start_MWMT_2021,]
LSR_daily_max_temp[end_MWMT_2021,]

#MWAT

LSR_daily_mean_temp_2021 <- LSR_daily_mean_temp %>% 
  filter(water_year == 2021)

LSR_2021_7DMean <- movingaves(LSR_daily_mean_temp_2021$daily_mean_C,7)
LSR_2021_7DMean <- data.frame(LSR_2021_7DMean)

MWAT_2021 <- max(LSR_2021_7DMean$LSR_2021_7DMean)

#get dates of MWAT
start_MWAT_2021 <- which(LSR_2021_7DMean$LSR_2021_7DMean==MWAT_2021)
end_MWAT_2021 <- start_MWAT_2021+6

LSR_daily_max_temp[start_MWAT_2021,]
LSR_daily_max_temp[end_MWAT_2021,]


# Plot daily max/mean/min temps -------------------------------------------

LSR_temp_all_QA$date <- date(LSR_temp_all_QA$date_time)

#make single dataframe w/ daily metrics
LSR_daily_2021 <- LSR_temp_all_QA %>% 
  filter(water_year > 2017) %>% 
  filter(water_year < 2022) %>% 
  group_by(date) %>% 
  summarize(daily_max = max(temp_C), daily_mean = mean(temp_C), daily_min = min(temp_C))

LSR_2021_plot <- LSR_daily_2021 %>% 
  gather(key = metric, value = temp_data, -c(date))

plot_colors <- c("daily_max" = "firebrick3", 
                 "daily_mean" = "goldenrod3", 
                 "daily_min" = "royalblue3")

ggplot() + geom_line(data = LSR_2021_plot, aes(x=date, y=temp_data, color = metric)) +
  labs(x = "", y = expression(paste("stream temp. ", "("^o, "C)"))) +
  scale_color_manual(values = plot_colors, labels = c("daily max", "daily mean", "daily min")) +
  scale_y_continuous(breaks = seq(5,25, by = 5)) +
  theme_bw() + 
  theme(text = element_text(size=10),
        legend.position = "bottom",
        legend.box.spacing = unit(0.25, "in"),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(-10,-10,10,-10),
        axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y = element_text(size = 8))

ggsave(filename = "output/LSR_WY2018-2021.png", dpi = 300, width = 6, height = 3, units = "in")
