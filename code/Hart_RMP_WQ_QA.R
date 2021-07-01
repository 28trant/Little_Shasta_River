
# Code description --------------------------------------------------------

# This code was developed to QA the water quality data for the Hart Ranch Management Plan update. IT only includes data for LSR. Constituents for the report include Temp, pH, EC, NH3, No3, No2, PO4, TP, DOC through water year 2020.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(lfstat)
library(plotly)


# Load data ---------------------------------------------------------------

Soda_Springs_WQ_raw <- read_csv("data/LSR_raw/LS_Hart_WQ_data.csv")


# Wrangle data -----------------------------------------------------

#Covert date from character to date
Soda_Springs_WQ_raw$Date <- as.POSIXct(Soda_Springs_WQ_raw$Date, format = "%m/%d/%Y", tz = Sys.timezone())

#Need to remove NAs from date, end at 2020 water year

Soda_Springs_WQ_filtered <- Soda_Springs_WQ_raw %>% 
  select(Date, ID, Tw_C, EC_us_cm, pH, `Turbidity-NTU`, `DOC-ppm`, `NH4-ppm`, `NO3-ppm`, TIN, `TN-ppm`, `PO4-ppm`, `TP-ppm`, `N:P`) %>% 
  filter(!is.na(Date) & Date != "2020-10-14") %>% 
  mutate(Date2 = ymd(Date)) %>% 
  mutate(water_year = water_year(Date2, origin = "usgs"))

#Change water year to numeric
Soda_Springs_WQ_filtered$water_year <- as.numeric(as.character(Soda_Springs_WQ_filtered$water_year))


# Plot data ---------------------------------------------------------------

#TN
Soda_Springs_TN <- Soda_Springs_WQ_filtered %>% 
  select(Date2, ID, `TN-ppm`, water_year) %>% 
  rename(Date = Date2, TN_ppm = `TN-ppm`) %>% 
  filter(ID == "LSR")

Soda_Springs_TN_ND <- Soda_Springs_TN %>% 
  filter(TN_ppm >= 0.1) #Detection limit TN = 0.1 mg/L

#start w/ base plot

base_TN <- ggplot(data = Soda_Springs_TN, aes(x = Date, y = TN_ppm)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0.1, color = "deeppink4") +
  labs(x = "date", y = "TN (mg/L)") +
  scale_fill_manual("ID", values = c("LSR" = "midnight blue")) +
  theme_classic()

base_TN

ggsave(filename = "output/Hart_RMP_TN_2018_2020.png", dpi = 300, width = 6, height = 2.5, units = "in")

#TIN
Soda_Springs_TIN <- Soda_Springs_WQ_filtered %>% 
  select(Date2, ID, TIN, water_year) %>% 
  rename(Date = Date2) %>% 
  filter(ID == "LSR")

#Check results that meet the detection limit NO3 > 0.05

Soda_Springs_TIN_ND <- Soda_Springs_TIN %>% 
  filter(TIN >= 0.05)

#start w/ base plot

base_TIN <- ggplot(data = Soda_Springs_TIN, aes(x = Date, y = TIN)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0.05, color = "deeppink4") +
  labs(x = "date", y = "TIN (mg/L)") +
  scale_fill_manual("ID", values = c("LSR" = "midnight blue")) +
  theme_classic()

base_TIN

ggsave(filename = "output/Hart_RMP_TIN_2018_2020.png", dpi = 300, width = 6, height = 2.5, units = "in")

#TP
Soda_Springs_TP <- Soda_Springs_WQ_filtered %>% 
  select(Date2, ID, `TP-ppm`, water_year) %>% 
  rename(Date = Date2, TP_ppm = `TP-ppm`) %>% 
  filter(ID == "LSR")

#Check results that meet the detection limit TP > 0.05 mg/L

Soda_Springs_TP_ND <- Soda_Springs_TP %>% 
  filter(TP_ppm >= 0.05)

#start w/ base plot

base_TP <- ggplot(data = Soda_Springs_TP, aes(x = Date, y = TP_ppm)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0.05, color = "deeppink4") +
  labs(x = "date", y = "TP (mg/L)") +
  scale_fill_manual("ID", values = c("LSR" = "midnight blue")) +
  theme_classic()

base_TP

ggsave(filename = "output/Hart_RMP_TP_2018_2020.png", dpi = 300, width = 6, height = 2.5, units = "in")

#TIP
Soda_Springs_TIP <- Soda_Springs_WQ_filtered %>% 
  select(Date2, ID, `PO4-ppm`, water_year) %>% 
  rename(Date = Date2, TIP_ppm = `PO4-ppm`) %>% 
  filter(ID == "LSR")

#Check for samples w/ concentrations abv detection limit TIP > 0.05

Soda_Springs_TIP_ND <- Soda_Springs_TIP %>% 
  filter(TIP_ppm >= 0.05)

#start w/ base plot

base_TIP <- ggplot(data = Soda_Springs_TIP, aes(x = Date, y = TIP_ppm)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0.05, color = "deeppink4") +
  labs(x = "date", y = "TIP (mg/L)") +
  scale_fill_manual("ID", values = c("LSR" = "midnight blue")) +
  theme_classic()

base_TIP

ggsave(filename = "output/Hart_RMP_TIP_2018_2020.png", dpi = 300, width = 6, height = 2.5, units = "in")

#DOC
Soda_Springs_DOC <- Soda_Springs_WQ_filtered %>% 
  select(Date2, ID, `DOC-ppm`, water_year) %>% 
  rename(Date = Date2, DOC_ppm = `DOC-ppm`) %>% 
  filter(ID == "LSR")

#Check for concentrations above detection limit DOC > 0.5 mg/L

Soda_Springs_DOC_ND <- Soda_Springs_DOC %>% 
  filter(DOC_ppm >= 0.5)

#start w/ base plot

base_DOC <- ggplot(data = Soda_Springs_DOC, aes(x = Date, y = DOC_ppm)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0.5, color = "deeppink4") +
  labs(x = "date", y = "DOC (mg/L)") +
  scale_fill_manual("ID", values = c("LSR" = "midnight blue")) +
  theme_classic()

base_DOC

ggsave(filename = "output/Hart_RMP_DOC_2018_2020.png", dpi = 300, width = 6, height = 2.5, units = "in")


# Summarize data ----------------------------------------------------------

min(Soda_Springs_TN$TN_ppm)
max(Soda_Springs_TN$TN_ppm)

max(Soda_Springs_TP_ND$TP_ppm)
max(Soda_Springs_TIP_ND$TIP_ppm)

max(Soda_Springs_DOC_ND$DOC_ppm)

#Summarize by season as in pre-project report (fall = Sep-Nov, winter = Dec-Feb, spring = Mar-May, summer = Jun-Aug)?

#Make facet plot by water year? If so, order the sites so that the plots show data from upstream to downstream, if showing all sites

#Soda_Springs_WQ_filtered$ID <- factor(Soda_Springs_WQ_filtered$ID, levels = c("LSR", "LSM", "LSH"))

# base_DOC +
#   facet_wrap(facets = vars(ID))