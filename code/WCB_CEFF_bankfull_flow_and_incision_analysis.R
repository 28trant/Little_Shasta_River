
# Code description --------------------------------------------------------

# This code was developed to calculate the bankfull flows of each cross-section that was drawn for the incision analysis in WCB_CEFF_lidar_xsxn.R. The purpose is to compare calculated bankfull flows with the Peak 2 flood flow. If bankfull flows are generally lower than the Peak 2, then there is no incision. If bankful flows are generally higher, then there is some incision that needs to be considered for the final functional flow recommendations.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(purrr)
library(plotly)
library(data.table) #used to extract minimum points from xsxns
library(sf) #used to load and read spatial data
library(mapview)
library(mapedit)

# Load data ---------------------------------------------------------------

xsxn_df <- read_rds("data/GIS/xsxn_elevation_w_scaled_positions.rds")
xsxn_sf <- read_rds("data/GIS/xsxn_elevation_w_coords_and_scaled_positions.rds")
LOI_segments <- read_rds("data/GIS/07_lshasta_loi_comids_flowline.rds")

LSR_LOIs_clip <- st_zm(LOI_segments)
# Map and plot cross-sections ---------------------------------------------

mapview(xsxn_sf, zcol= "X_leaflet_id", legend = FALSE) +
  mapview(LSR_LOIs_clip)

ggplotly(
  ggplot(data = xsxn_df) +
    geom_line(aes(x=location, y=elevation_m, group = X_leaflet_id, color = X_leaflet_id))
)


# Next steps:
# Filter xsxns to only include those that overlap on the comids for LOIs 1, 2, and 3. Add slope and Manning's n to each data frame.
#      For each cross-section:
# Identify bankful segment by comparing the plotly line with the map to confirm channel location; 
# Calculate area and hydraulic radius; area is area of a trapezoid, radius is length of 3 sides. May need to use triangle formula to calculate diagonal bank distance.
# Area of trapezoid: (1/2)*(a+b)*h where b = length of base and a = length of top side
# For slope, use the slope of the comids
# Calculate flow (Q) using Manning's equation: 
#      If using U.S. units: Q = (1.49/n)*A*R^(2/3)*S^(1/2)
#      If using S.I. units: Q = (1.00/n)*A*R^(2/3)*S^(1/2)