
# Code description --------------------------------------------------------

# This code runs the flow data from the LSR gage on CDEC through the functional flow calculator. Because there are only four water years available, we only want to look at individual year's metrics, not average metrics overall.


# Libraries ---------------------------------------------------------------

library(ffcAPIClient)
library(tidyverse)
library(lubridate)


# Load data ---------------------------------------------------------------

LSR_flow <- read_csv("data/LSR_processed/LSR_flow_2017_2020.csv")

Ann_token <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJBbm4iLCJsYXN0TmFtZSI6IldpbGxpcyIsImVtYWlsIjoiYXdpbGxpc0B1Y2RhdmlzLmVkdSIsInJvbGUiOiJVU0VSIiwiaWF0IjoxNjA1NjUxMTc4fQ.MzMJ23D6tRmOD9Sr3OcQoCVLLndsV2w5ZKdzwgDOeOM"

LSR_gage_comid <- 3917198

#Wrangle into correct format for ffc (See sierra_pywr code for guidance)
LSR_flow_filtered <- LSR_flow %>% 
  select(date, daily_mean_cfs) %>% 
  rename("flow" = "daily_mean_cfs")

LSR_flow_filtered$date <- ymd(LSR_flow_filtered$date)

# Run ffc -----------------------------------------------------------------

ffc_lsr$date_format_string <- "%Y-%m-%d"

ffc_lsr <- FFCProcessor$new() # make a new object we can use to run the commands

ffc_lsr$step_one_functional_flow_results(timeseries = LSR_flow_filtered,
                                         token = Ann_token,
                                         comid = LSR_gage_comid,
                                         output_folder = "functional_flows/LSR_gage")
