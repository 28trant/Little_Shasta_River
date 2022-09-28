
# Code description --------------------------------------------------------

# This code was developed to create a map of the cross-sections used in the incision analysis for the CEFF: Little Shasta case study.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(purrr)
library(sf) #used to load and read spatial data
library(mapview)


# Load data ---------------------------------------------------------------

xsxn_df <- read_rds("data/GIS/xsxn_elevation_w_scaled_positions.rds")
xsxn_sf <- read_rds("data/GIS/xsxn_elevation_w_coords_and_scaled_positions.rds")
LOI_segments <- read_rds("data/GIS/07_lshasta_loi_comids_flowline.rds")
LOI_geometry_sf <- read_rds("data/GIS/loi_slope_nhd_streamlines.rds")

LSR_LOIs_clip <- st_zm(LOI_segments)

# May need to add other files from Ryan's code to make the final map.

# Wrangle xsxns to only those in LOIs -------------------------------------

xsxn_sf_LOI1 <- xsxn_sf %>% 
  filter(X_leaflet_id >= 2999) %>% 
  rename(xsxn_id = X_leaflet_id)

xsxn_sf_LOI2 <- xsxn_sf %>% 
  filter(2499 >= X_leaflet_id & X_leaflet_id >= 1942) %>% 
  rename(xsxn_id = X_leaflet_id)

xsxn_sf_LOI3 <- xsxn_sf %>% 
  filter(1239 >= X_leaflet_id & X_leaflet_id >= 1110) %>% 
  rename(xsxn_id = X_leaflet_id)

xsxn_sf_LOI1_2 <- rbind(xsxn_sf_LOI1, xsxn_sf_LOI2)

xsxn_sf_LOI_all <- rbind(xsxn_sf_LOI1_2, xsxn_sf_LOI3)


# Make plots --------------------------------------------------------------

mapview(xsxn_sf_LOI_all, zcol= "xsxn_id", legend = FALSE) +
  mapview(LSR_LOIs_clip)

# For some reason, this isn't working. Will try to export as a shp file and have Amber plot it.

st_write(xsxn_sf_LOI_all, "output/xsxn_sf_LOI_all.shp")
