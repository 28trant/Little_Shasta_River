
# Code description --------------------------------------------------------

# This code was developed to QA the water quality data for the Hart Ranch Management Plan update. IT only includes data for LSR. Constituents for the report include Temp, pH, EC, NH3, No3, No2, PO4, TP, DOC through water year 2020.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(plotly)


# Load data ---------------------------------------------------------------

Soda_Springs_WQ <- read_csv("data/LSR_raw/LS_Hart_WQ_data.csv")


# Wrangle data -----------------------------------------------------

#Covert date from character to date
Soda_Springs_WQ$Date <- as.POSIXct(Soda_Springs_WQ$Date, format = "%m/%d/%Y", tz = Sys.timezone())

#Need to remove NAs from date, clean up the rest of the data
