
# Code description --------------------------------------------------------

# This code takes the model results from W3T analysis of alternative water right transfers (Evans Springs versus Little Shasta) and plots the results.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(plotly)
library(cowplot)

# Load data ---------------------------------------------------------------

W3T_results <- read_csv("data/W3T_results/ALL_SimulationAssessments_7-day_v2.csv")

#make sample dataset and reformat dates/times
Obs_data <- W3T_results %>% 
  select(Obs_TimeOut, Obs_Twout) %>% 
  mutate(date_time = mdy_hm(Obs_TimeOut))

#Test to make sure it plots correctly
ggplotly(
  ggplot(data=Obs_data) +
    geom_line(aes(x=date_time, y=Obs_Twout))
)

#reformat remaining dates/times
Base_results <- W3T_results %>% 
  select(Base_TimeOut,Base_TWout) %>%
  filter(!is.na(Base_TWout)) %>% 
  mutate(date_time = mdy_hm(Base_TimeOut))

#There's something weird-looking about the results starting 8/23/2020 for all results; end results on 8/22/2020 for plotting for all results
Base_results_clip <- Base_results %>% 
  filter(date_time < ymd("2020-08-23"))

HC_results <- W3T_results %>% 
  select(HC_TimeOut, HC_TWout) %>%
  filter(!is.na(HC_TWout)) %>% 
  mutate(date_time = mdy_hm(HC_TimeOut))

HC_results_clip <- HC_results %>% 
  filter(date_time < ymd("2020-08-23"))

HR_results <- W3T_results %>% 
  select(HR_TimeOut, HR_TWout) %>%
  filter(!is.na(HR_TWout)) %>%  
  mutate(date_time = mdy_hm(HR_TimeOut))

HR_results_clip <- HR_results %>% 
  filter(date_time < ymd("2020-08-23"))

PipeA_results <- W3T_results %>% 
  select(PipeA_TimeOut, PipeA_TWout) %>%
  filter(!is.na(PipeA_TWout)) %>%  
  mutate(date_time = mdy_hm(PipeA_TimeOut))

PipeA_results_clip <- PipeA_results %>% 
  filter(date_time < ymd("2020-08-23"))

PipeB_results <- W3T_results %>%
  select(PipeB_TimeOut, PipeB_TWout) %>%
  filter(!is.na(PipeB_TWout)) %>%  
  mutate(date_time = mdy_hm(PipeB_TimeOut))

PipeB_results_clip <- PipeB_results %>% 
  filter(date_time < ymd("2020-08-23"))

Runoff_results <- W3T_results %>% 
  select(Null_TimeOut, Null_TWout) %>%
  filter(!is.na(Null_TWout)) %>%  
  rename(Runoff_TWout = Null_TWout) %>%
  mutate(date_time = mdy_hm(Null_TimeOut))

Runoff_results_clip <- Runoff_results %>% 
  filter(date_time < ymd("2020-08-23"))


# Plot all model results

#Plot the baseline (observed scenario), which we'll use to add each of the other results with unique titles
Base_plot <- ggplot(data = Base_results_clip) +
  geom_line(aes(x=date_time, y=Base_TWout), color = "grey27", linetype = "dashed", size = 1) +
  scale_x_datetime(date_breaks = "1 day") +
  scale_y_continuous(limits = c(10,21)) +
  labs(x=" ",
       y=expression("water temperature " (degree*C))) +
  theme_bw()

#Check the plot object to make sure it plots correctly:
Base_plot

#Evans Spring transfer: historical channel
Base_plot +
  geom_line(data = HC_results_clip, aes(x=date_time, y=HC_TWout), color = "blue4", size = 1) +
  labs(title = "Evans Spring transfer: historical channel")

#save
ggsave(filename = "output/W3T_Evans/Evans_Spring_historical_channel.jpg", width = 6, height = 4, units = "in", dpi = 300)

#Evans Spring transfer: restored channel
Base_plot +
  geom_line(data = HR_results_clip, aes(x=date_time, y=HR_TWout), color = "blue4", size = 1) +
  labs(title = "Evans Spring transfer: restored channel")

#save
ggsave(filename = "output/W3T_Evans/Evans_Spring_restored_channel.jpg", width = 6, height = 4, units = "in", dpi = 300)

#Evans Spring transfer: pipe to upstream boundary
Base_plot +
  geom_line(data = PipeA_results_clip, aes(x=date_time, y=PipeA_TWout), color = "blue4", size = 1) +
  labs(title = "Evans Spring transfer: pipe to upstream boundary")

#save
ggsave(filename = "output/W3T_Evans/Pipe_A.jpg", width = 6, height = 4, units = "in", dpi = 300)

#Evans Spring transfer: pipe to historical confluence
Base_plot +
  geom_line(data = PipeB_results_clip, aes(x=date_time, y=PipeB_TWout), color = "blue4", size = 1) +
  labs(title = "Evans Spring transfer: pipe to historical confluence")

#save
ggsave(filename = "output/W3T_Evans/Pipe_B.jpg", width = 6, height = 4, units = "in", dpi = 300)

#Runoff water transfer, no spring water
Base_plot +
  geom_line(data = Runoff_results_clip, aes(x=date_time, y=Runoff_TWout), color = "blue4", size = 1) +
  labs(title = "Runoff transfer: no spring water")

#save
ggsave(filename = "output/W3T_Evans/Runoff.jpg", width = 6, height = 4, units = "in", dpi = 300)
