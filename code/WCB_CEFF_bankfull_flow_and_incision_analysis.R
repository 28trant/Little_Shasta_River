
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
LOI_geometry_sf <- read_rds("data/GIS/loi_slope_nhd_streamlines.rds")

LSR_LOIs_clip <- st_zm(LOI_segments)
LOI_geometry <- st_drop_geometry(LOI_geometry_sf)

# Map and plot cross-sections ---------------------------------------------

mapview(xsxn_sf, zcol= "X_leaflet_id", legend = FALSE) +
  mapview(LSR_LOIs_clip)

ggplotly(
  ggplot(data = xsxn_df) +
    geom_line(aes(x=location, y=elevation_m, group = X_leaflet_id, color = X_leaflet_id))
)



# Wrangle xsxn data for each LOI ------------------------------------------
# Filter xsxns to only include those that overlap on the comids for LOIs 1, 2, and 3. Add slope and Manning's n to each data frame. Slope based on data extracted from comid geometry. Estimate Manning's n from Chow (1959).

#Filter for xsxns in LOI3, clean up dataframe
xsxn_LOI3_all <- xsxn_df %>% 
  filter(X_leaflet_id >= 2999) %>% 
  select(X_leaflet_id, location, elevation_m) %>% 
  rename(xsxn_id = X_leaflet_id, location_m = location)

xsxn_LOI3 <- xsxn_LOI3_all %>%
  select(xsxn_id) %>% 
  distinct(xsxn_id) 
  
  
#Add LOI identifier; check xsxn count against mapped xsxns
xsxn_LOI3$LOI_id <- "LOI_3"
unique(xsxn_LOI3$xsxn_id)

#Add slope
LOI3_slope <- LOI_geometry[LOI_geometry$comid == "3917946", 2]
xsxn_LOI3$slope <- LOI3_slope

#Add Manning's n value; LOI3 = Floodplain, pasture, no brush, short grass
xsxn_LOI3$Manning_n <- 0.030

#Filter for xsxns in LOI2, clean up dataframe
xsxn_LOI2_all <- xsxn_df %>% 
  filter(2499 >= X_leaflet_id & X_leaflet_id >= 1942) %>% 
  select(X_leaflet_id, location, elevation_m) %>% 
  rename(xsxn_id = X_leaflet_id, location_m = location)

xsxn_LOI2 <- xsxn_LOI2_all %>% 
  select(xsxn_id) %>%
  distinct(xsxn_id) 

#Add LOI identifier; check xsxn count against mapped xsxns
xsxn_LOI2$LOI_id <- "LOI_2"
unique(xsxn_LOI2$xsxn_id)

#Add slope
LOI2_slope <- LOI_geometry[LOI_geometry$comid == "3917950", 2]
xsxn_LOI2$slope <- LOI2_slope

#Add Manning's n value; LOI2 = Main channel, lower stages, more ineffective slopes and sections
xsxn_LOI2$Manning_n <- 0.048

#Filter for xswxns in LOI3, clean up dataframe
xsxn_LOI1_all <- xsxn_df %>% 
  filter(1239 >= X_leaflet_id & X_leaflet_id >= 1110) %>% 
  select(X_leaflet_id, location, elevation_m) %>% 
  rename(xsxn_id = X_leaflet_id, location_m = location)

xsxn_LOI1 <- xsxn_LOI1_all %>% 
  select(xsxn_id) %>%
  distinct(xsxn_id) 

#Add LOI identifier, check xsxn count against mapped xsxns 
xsxn_LOI1$LOI_id <- "LOI_1"
unique(xsxn_LOI1$xsxn_id)

#Add slope
LOI1_slope <- LOI_geometry[LOI_geometry$comid == "3917198", 2]
xsxn_LOI1$slope <- LOI1_slope

#Add Manning's n value; LOI1 = main channel, clean, straight, some stones and weeds
xsxn_LOI1$Manning_n <- 0.035


# Identify bankfull geometry ----------------------------------------------

# Identify bankfull segment by comparing the plotly line with the map to confirm channel location. Bankfull geometry includes hydraulic radius (wetted perimeter) and area variables. Wetted perimeter includes length of bankfull channel (3 sides); may need to use Pythagoreum theorum (c^2 = a^2 + b^2) to calculate diagonal bank distance. Area likely to be area of trapezoid: (1/2)*(a+b)*h where b = length of base and a = length of top side.

# Start with LOI 1

#Plot cross-sections

ggplotly(
  ggplot(data = xsxn_LOI1_all) +
    geom_line(aes(x=location_m, y=elevation_m, group = xsxn_id, color = xsxn_id))
)

# Add columns to geometry data
xsxn_LOI1$BF_L_m <- 0.0
xsxn_LOI1$BF_R_m <- 0.0
xsxn_LOI1$BF_L_h_m <- 0.0
xsxn_LOI1$BF_L_w_m <- 0.0
xsxn_LOI1$BF_R_h_m <- 0.0
xsxn_LOI1$BF_R_w_m <- 0.0
xsxn_LOI1$xsxn_BF_L_length_m <- 0.0
xsxn_LOI1$xsxn_BF_R_length_m <- 0.0
xsxn_LOI1$xsxn_BF_base_length_m <- 0.0
xsxn_LOI1$xsxn_BF_top_length_m <- 0.0


#xsxn 1110
# define end points of bankfull channel
xsxn_1110_BF_L <- 38.4048
xsxn_1110_BF_R <- 41.3
xsxn_LOI1[xsxn_LOI1$xsxn_id==1110, 5:6] <- as.list(c(xsxn_1110_BF_L, xsxn_1110_BF_R))

#filter points for easier calculations
BF_xsxn_1110 <- xsxn_LOI1_all %>%
  filter(xsxn_id == 1110) %>% 
  filter(location_m >= 38.4048 & location_m <= 41.3)

#Define other geometry data
#BF_L_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==1110, 7] <- 2910.024-min(BF_xsxn_1110$elevation_m)

#BF_L_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==1110, 8] <- 39.7764-38.4048

#BF_R_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==1110, 9] <- 2909.835-min(BF_xsxn_1110$elevation_m)

#BF_R_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==1110, 10] <- 41.3-40.6908

#BF_base_length
xsxn_LOI1[xsxn_LOI1$xsxn_id==1110, 13] <- 40.6908-39.7764

# Repeat for cross-section 1239


# Calculate bankfull flow -------------------------------------------------

#When all data is identified for each cross-section, calculate:
#BF_height: choose the minimum of the side heights 
xsxn_LOI1$BF_h_m <- pmin(xsxn_LOI1$BF_L_h_m, xsxn_LOI1$BF_R_h_m)

#BF_top_length
xsxn_LOI1$xsxn_BF_top_length_m <- xsxn_LOI1$BF_R_m - xsxn_LOI1$BF_L_m

#BF_L_length
xsxn_LOI1$xsxn_BF_L_length_m <- sqrt((xsxn_LOI1$BF_L_h_m)^2+(xsxn_LOI1$BF_L_w_m)^2)

#BF_R_length
xsxn_LOI1$xsxn_BF_R_length_m <- sqrt((xsxn_LOI1$BF_R_h_m)^2+(xsxn_LOI1$BF_R_w_m)^2)

# Wetted perimeter
xsxn_LOI1$R_hyd_rad <- xsxn_LOI1$xsxn_BF_L_length_m+xsxn_LOI1$xsxn_BF_R_length_m+xsxn_LOI1$xsxn_BF_base_length_m

#Bankfull area
xsxn_LOI1$Area <- (1/2)*(xsxn_LOI1$xsxn_BF_top_length_m+xsxn_LOI1$xsxn_BF_base_length_m)*xsxn_LOI1$BF_h_m

#Bankfull flow
# Calculate flow (Q) using Manning's equation: 
#      If using U.S. units: Q = (1.49/n)*A*R^(2/3)*S^(1/2)
#      If using S.I. units: Q = (1.00/n)*A*R^(2/3)*S^(1/2)
#Units are all metric, so will use the SI version of Manning's equation and convert to cfs using 1 cms = 35.315 cfs.

xsxn_LOI1$BF_flow_cfs <- (1.00/xsxn_LOI1$Manning_n)*xsxn_LOI1$Area*xsxn_LOI1$R_hyd_rad^(2/3)*xsxn_LOI1$slope^(1/2)*35.315

