
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

#Filter for xsxns in LOI1, clean up dataframe
xsxn_LOI1_all <- xsxn_df %>% 
  filter(X_leaflet_id >= 2999) %>% 
  select(X_leaflet_id, location, elevation_m) %>% 
  rename(xsxn_id = X_leaflet_id, location_m = location)

xsxn_LOI1 <- xsxn_LOI1_all %>%
  select(xsxn_id) %>% 
  distinct(xsxn_id) 
  
  
#Add LOI identifier; check xsxn count against mapped xsxns
xsxn_LOI1$LOI_id <- "LOI_3"
unique(xsxn_LOI1$xsxn_id)

#Add slope
LOI1_slope <- LOI_geometry[LOI_geometry$comid == "3917946", 2]
xsxn_LOI1$slope <- LOI1_slope

#Add Manning's n value; LOI1 = Floodplain, pasture, no brush, short grass
xsxn_LOI1$Manning_n <- 0.030

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
xsxn_LOI3_all <- xsxn_df %>% 
  filter(1239 >= X_leaflet_id & X_leaflet_id >= 1110) %>% 
  select(X_leaflet_id, location, elevation_m) %>% 
  rename(xsxn_id = X_leaflet_id, location_m = location)

xsxn_LOI3 <- xsxn_LOI3_all %>% 
  select(xsxn_id) %>%
  distinct(xsxn_id) 

#Add LOI identifier, check xsxn count against mapped xsxns 
xsxn_LOI3$LOI_id <- "LOI_1"
unique(xsxn_LOI3$xsxn_id)

#Add slope
LOI3_slope <- LOI_geometry[LOI_geometry$comid == "3917198", 2]
xsxn_LOI3$slope <- LOI3_slope

#Add Manning's n value; LOI3 = main channel, clean, straight, some stones and weeds
xsxn_LOI3$Manning_n <- 0.035


# LOI3: Identify bankfull geometry ----------------------------------------------

# Identify bankfull segment by comparing the plotly line with the map to confirm channel location. Bankfull geometry includes hydraulic radius (wetted perimeter) and area variables. Wetted perimeter includes length of bankfull channel (3 sides); may need to use Pythagoreum theorum (c^2 = a^2 + b^2) to calculate diagonal bank distance. Area likely to be area of trapezoid: (1/2)*(a+b)*h where b = length of base and a = length of top side.

# Start with LOI 3

#Plot cross-sections

ggplotly(
  ggplot(data = xsxn_LOI3_all) +
    geom_line(aes(x=location_m, y=elevation_m, group = xsxn_id, color = xsxn_id))
)

# Add columns to geometry data
xsxn_LOI3$BF_L_m <- 0.0
xsxn_LOI3$BF_R_m <- 0.0
xsxn_LOI3$BF_L_h_m <- 0.0
xsxn_LOI3$BF_L_w_m <- 0.0
xsxn_LOI3$BF_R_h_m <- 0.0
xsxn_LOI3$BF_R_w_m <- 0.0
xsxn_LOI3$xsxn_BF_L_length_m <- 0.0
xsxn_LOI3$xsxn_BF_R_length_m <- 0.0
xsxn_LOI3$xsxn_BF_base_length_m <- 0.0
xsxn_LOI3$xsxn_BF_top_length_m <- 0.0


#xsxn 1110
# define end points of bankfull channel
xsxn_1110_BF_L <- 38.4048
xsxn_1110_BF_R <- 41.3
xsxn_LOI3[xsxn_LOI3$xsxn_id==1110, 5:6] <- as.list(c(xsxn_1110_BF_L, xsxn_1110_BF_R))

#filter points for easier calculations
BF_xsxn_1110 <- xsxn_LOI3_all %>%
  filter(xsxn_id == 1110) %>% 
  filter(location_m >= 38.4048 & location_m <= 41.3)

#Define other geometry data
#BF_L_height_m
xsxn_LOI3[xsxn_LOI3$xsxn_id==1110, 7] <- 2910.024-min(BF_xsxn_1110$elevation_m)

#BF_L_width_m
xsxn_LOI3[xsxn_LOI3$xsxn_id==1110, 8] <- 39.7764-38.4048

#BF_R_height_m
xsxn_LOI3[xsxn_LOI3$xsxn_id==1110, 9] <- 2909.835-min(BF_xsxn_1110$elevation_m)

#BF_R_width_m
xsxn_LOI3[xsxn_LOI3$xsxn_id==1110, 10] <- 41.3-40.6908

#BF_base_length
xsxn_LOI3[xsxn_LOI3$xsxn_id==1110, 13] <- 40.6908-39.7764

# Repeat for cross-section 1239
#xsxn 1239
# define end points of bankfull channel
xsxn_1239_BF_L <- 28.3464
xsxn_1239_BF_R <- 33.3756
xsxn_LOI3[xsxn_LOI3$xsxn_id==1239, 5:6] <- as.list(c(xsxn_1239_BF_L, xsxn_1239_BF_R))

#filter points for easier calculations
BF_xsxn_1239 <- xsxn_LOI3_all %>%
  filter(xsxn_id == 1239) %>% 
  filter(location_m >= xsxn_1239_BF_L & location_m <= xsxn_1239_BF_R)

#Define other geometry data
#BF_L_height_m
xsxn_LOI3[xsxn_LOI3$xsxn_id==1239, 7] <- 2877.122-min(BF_xsxn_1239$elevation_m)

#BF_L_width_m
xsxn_LOI3[xsxn_LOI3$xsxn_id==1239, 8] <- 30.6324-xsxn_1239_BF_L

#BF_R_height_m
xsxn_LOI3[xsxn_LOI3$xsxn_id==1239, 9] <- 2877.101-min(BF_xsxn_1239$elevation_m)

#BF_R_width_m
xsxn_LOI3[xsxn_LOI3$xsxn_id==1239, 10] <- xsxn_1239_BF_R-32.9184

#BF_base_length
xsxn_LOI3[xsxn_LOI3$xsxn_id==1239, 13] <- 32.9184-30.6324

# LOI3: Calculate bankfull flow -------------------------------------------------

#When all data is identified for each cross-section, calculate:
#BF_height: choose the minimum of the side heights 
xsxn_LOI3$BF_h_m <- pmin(xsxn_LOI3$BF_L_h_m, xsxn_LOI3$BF_R_h_m)

#BF_top_length
xsxn_LOI3$xsxn_BF_top_length_m <- xsxn_LOI3$BF_R_m - xsxn_LOI3$BF_L_m

#BF_L_length
xsxn_LOI3$xsxn_BF_L_length_m <- sqrt((xsxn_LOI3$BF_L_h_m)^2+(xsxn_LOI3$BF_L_w_m)^2)

#BF_R_length
xsxn_LOI3$xsxn_BF_R_length_m <- sqrt((xsxn_LOI3$BF_R_h_m)^2+(xsxn_LOI3$BF_R_w_m)^2)

# Wetted perimeter
xsxn_LOI3$R_hyd_rad <- xsxn_LOI3$xsxn_BF_L_length_m+xsxn_LOI3$xsxn_BF_R_length_m+xsxn_LOI3$xsxn_BF_base_length_m

#Bankfull area
xsxn_LOI3$Area <- (1/2)*(xsxn_LOI3$xsxn_BF_top_length_m+xsxn_LOI3$xsxn_BF_base_length_m)*xsxn_LOI3$BF_h_m

#Bankfull flow
# Calculate flow (Q) using Manning's equation: 
#      If using U.S. units: Q = (1.49/n)*A*R^(2/3)*S^(1/2)
#      If using S.I. units: Q = (1.00/n)*A*R^(2/3)*S^(1/2)
#Units are all metric, so will use the SI version of Manning's equation; the 1.00 is the k conversion factor to calculate flow in cfs.

xsxn_LOI3$BF_flow_cfs <- (1.00/xsxn_LOI3$Manning_n)*xsxn_LOI3$Area*xsxn_LOI3$R_hyd_rad^(2/3)*xsxn_LOI3$slope^(1/2)

# LOI2: Identify bankfull geometry ----------------------------------------------

# Identify bankfull segment by comparing the plotly line with the map to confirm channel location. Bankfull geometry includes hydraulic radius (wetted perimeter) and area variables. Wetted perimeter includes length of bankfull channel (3 sides); may need to use Pythagoreum theorum (c^2 = a^2 + b^2) to calculate diagonal bank distance. Area likely to be area of trapezoid: (1/2)*(a+b)*h where b = length of base and a = length of top side.

#Plot cross-sections

ggplotly(
  ggplot(data = xsxn_LOI2_all) +
    geom_line(aes(x=location_m, y=elevation_m, group = xsxn_id, color = xsxn_id))
)

# Add columns to geometry data
xsxn_LOI2$BF_L_m <- 0.0
xsxn_LOI2$BF_R_m <- 0.0
xsxn_LOI2$BF_L_h_m <- 0.0
xsxn_LOI2$BF_L_w_m <- 0.0
xsxn_LOI2$BF_R_h_m <- 0.0
xsxn_LOI2$BF_R_w_m <- 0.0
xsxn_LOI2$xsxn_BF_L_length_m <- 0.0
xsxn_LOI2$xsxn_BF_R_length_m <- 0.0
xsxn_LOI2$xsxn_BF_base_length_m <- 0.0
xsxn_LOI2$xsxn_BF_top_length_m <- 0.0

#xsxn 1942
# define end points of bankfull channel
xsxn_1942_BF_L <- 41.6052
xsxn_1942_BF_R <- 52.1208
xsxn_LOI2[xsxn_LOI2$xsxn_id==1942, 5:6] <- as.list(c(xsxn_1942_BF_L, xsxn_1942_BF_R))

#filter points for easier calculations
BF_xsxn_1942 <- xsxn_LOI2_all %>%
  filter(xsxn_id == 1942) %>% 
  filter(location_m >= xsxn_1942_BF_L & location_m <= xsxn_1942_BF_R)

#Define other geometry data
#BF_L_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==1942, 7] <- 2715.950-min(BF_xsxn_1942$elevation_m)

#BF_L_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==1942, 8] <- 45.7200-xsxn_1942_BF_L

#BF_R_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==1942, 9] <- 2716.860-min(BF_xsxn_1942$elevation_m)

#BF_R_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==1942, 10] <- xsxn_1942_BF_R-45.7200

#BF_base_length
xsxn_LOI2[xsxn_LOI2$xsxn_id==1942, 13] <- 48.0060-45.7200

#xsxn 2085
# define end points of bankfull channel
xsxn_2085_BF_L <- 23.7744
xsxn_2085_BF_R <- 44.8056
xsxn_LOI2[xsxn_LOI2$xsxn_id==2085, 5:6] <- as.list(c(xsxn_2085_BF_L, xsxn_2085_BF_R))

#filter points for easier calculations
BF_xsxn_2085 <- xsxn_LOI2_all %>%
  filter(xsxn_id == 2085) %>% 
  filter(location_m >= xsxn_2085_BF_L & location_m <= xsxn_2085_BF_R)

#Define other geometry data
#BF_L_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2085, 7] <- 2712.632-min(BF_xsxn_2085$elevation_m)

#BF_L_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2085, 8] <- 31.0896-xsxn_2085_BF_L

#BF_R_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2085, 9] <- 2712.447-min(BF_xsxn_2085$elevation_m)

#BF_R_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2085, 10] <- xsxn_2085_BF_R-34.2900

#BF_base_length
xsxn_LOI2[xsxn_LOI2$xsxn_id==2085, 13] <- 34.2900-31.0896

#xsxn 2103
# define end points of bankfull channel
xsxn_2103_BF_L <- 17.3736
xsxn_2103_BF_R <- 28.3464
xsxn_LOI2[xsxn_LOI2$xsxn_id==2103, 5:6] <- as.list(c(xsxn_2103_BF_L, xsxn_2103_BF_R))

#filter points for easier calculations
BF_xsxn_2103 <- xsxn_LOI2_all %>%
  filter(xsxn_id == 2103) %>% 
  filter(location_m >= xsxn_2103_BF_L & location_m <= xsxn_2103_BF_R)

#Define other geometry data
#BF_L_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2103, 7] <- 2702.837-min(BF_xsxn_2103$elevation_m)

#BF_L_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2103, 8] <- 21.4884-xsxn_2103_BF_L

#BF_R_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2103, 9] <- 2702.874-min(BF_xsxn_2103$elevation_m)

#BF_R_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2103, 10] <- xsxn_2103_BF_R-23.7744

#BF_base_length
xsxn_LOI2[xsxn_LOI2$xsxn_id==2103, 13] <- 23.7744-21.4884

#xsxn 2119
# define end points of bankfull channel
xsxn_2119_BF_L <- 15.5448
xsxn_2119_BF_R <- 32.4612
xsxn_LOI2[xsxn_LOI2$xsxn_id==2119, 5:6] <- as.list(c(xsxn_2119_BF_L, xsxn_2119_BF_R))

#filter points for easier calculations
BF_xsxn_2119 <- xsxn_LOI2_all %>%
  filter(xsxn_id == 2119) %>% 
  filter(location_m >= xsxn_2119_BF_L & location_m <= xsxn_2119_BF_R)

#Define other geometry data
#BF_L_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2119, 7] <- 2693.511-min(BF_xsxn_2119$elevation_m)

#BF_L_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2119, 8] <- 26.9748-xsxn_2119_BF_L

#BF_R_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2119, 9] <- 2693.543-min(BF_xsxn_2119$elevation_m)

#BF_R_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2119, 10] <- xsxn_2119_BF_R-27.8892

#BF_base_length
xsxn_LOI2[xsxn_LOI2$xsxn_id==2119, 13] <- 27.8892-26.9748

#xsxn 2200
# define end points of bankfull channel
xsxn_2200_BF_L <- 12.3444
xsxn_2200_BF_R <- 32.0040
xsxn_LOI2[xsxn_LOI2$xsxn_id==2200, 5:6] <- as.list(c(xsxn_2200_BF_L, xsxn_2200_BF_R))

#filter points for easier calculations
BF_xsxn_2200 <- xsxn_LOI2_all %>%
  filter(xsxn_id == 2200) %>% 
  filter(location_m >= xsxn_2200_BF_L & location_m <= xsxn_2200_BF_R)

#Define other geometry data
#BF_L_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2200, 7] <- 2686.318-min(BF_xsxn_2200$elevation_m)

#BF_L_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2200, 8] <- 20.5740-xsxn_2200_BF_L

#BF_R_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2200, 9] <- 2686.055-min(BF_xsxn_2200$elevation_m)

#BF_R_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2200, 10] <- xsxn_2200_BF_R-22.8600

#BF_base_length
xsxn_LOI2[xsxn_LOI2$xsxn_id==2200, 13] <- 22.8600-20.5740

#xsxn 2217
# define end points of bankfull channel
xsxn_2217_BF_L <- 29.7180
xsxn_2217_BF_R <- 33.3756
xsxn_LOI2[xsxn_LOI2$xsxn_id==2217, 5:6] <- as.list(c(xsxn_2217_BF_L, xsxn_2217_BF_R))

#filter points for easier calculations
BF_xsxn_2217 <- xsxn_LOI2_all %>%
  filter(xsxn_id == 2217) %>% 
  filter(location_m >= xsxn_2217_BF_L & location_m <= xsxn_2217_BF_R)

#Define other geometry data
#BF_L_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2217, 7] <- 2675.719-min(BF_xsxn_2217$elevation_m)

#BF_L_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2217, 8] <- 30.6324-xsxn_2217_BF_L

#BF_R_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2217, 9] <- 2675.745-min(BF_xsxn_2217$elevation_m)

#BF_R_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2217, 10] <- xsxn_2217_BF_R-31.5468

#BF_base_length
xsxn_LOI2[xsxn_LOI2$xsxn_id==2217, 13] <- 31.5468-30.6324

#xsxn 2231
# define end points of bankfull channel
xsxn_2231_BF_L <- 28.8036
xsxn_2231_BF_R <- 44.8056
xsxn_LOI2[xsxn_LOI2$xsxn_id==2231, 5:6] <- as.list(c(xsxn_2231_BF_L, xsxn_2231_BF_R))

#filter points for easier calculations
BF_xsxn_2231 <- xsxn_LOI2_all %>%
  filter(xsxn_id == 2231) %>% 
  filter(location_m >= xsxn_2231_BF_L & location_m <= xsxn_2231_BF_R)

#Define other geometry data
#BF_L_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2231, 7] <- 2667.230-min(BF_xsxn_2231$elevation_m)

#BF_L_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2231, 8] <- 31.0896-xsxn_2231_BF_L

#BF_R_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2231, 9] <- 2667.252-min(BF_xsxn_2231$elevation_m)

#BF_R_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2231, 10] <- xsxn_2231_BF_R-38.8620

#BF_base_length
xsxn_LOI2[xsxn_LOI2$xsxn_id==2231, 13] <- 38.8620-31.0896

#xsxn 2244
# define end points of bankfull channel
xsxn_2244_BF_L <- 32.0040
xsxn_2244_BF_R <- 34.7472
xsxn_LOI2[xsxn_LOI2$xsxn_id==2244, 5:6] <- as.list(c(xsxn_2244_BF_L, xsxn_2244_BF_R))

#filter points for easier calculations
BF_xsxn_2244 <- xsxn_LOI2_all %>%
  filter(xsxn_id == 2244) %>% 
  filter(location_m >= xsxn_2244_BF_L & location_m <= xsxn_2244_BF_R)

#Define other geometry data
#BF_L_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2244, 7] <- 2656.006-min(BF_xsxn_2244$elevation_m)

#BF_L_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2244, 8] <- 33.8328-xsxn_2244_BF_L

#BF_R_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2244, 9] <- 2656.040-min(BF_xsxn_2244$elevation_m)

#BF_R_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2244, 10] <- xsxn_2244_BF_R-34.2900

#BF_base_length
xsxn_LOI2[xsxn_LOI2$xsxn_id==2244, 13] <- 34.2900-33.832

#xsxn 2305
# define end points of bankfull channel
xsxn_2305_BF_L <- 26.9748
xsxn_2305_BF_R <- 34.7472
xsxn_LOI2[xsxn_LOI2$xsxn_id==2305, 5:6] <- as.list(c(xsxn_2305_BF_L, xsxn_2305_BF_R))

#filter points for easier calculations
BF_xsxn_2305 <- xsxn_LOI2_all %>%
  filter(xsxn_id == 2305) %>% 
  filter(location_m >= xsxn_2305_BF_L & location_m <= xsxn_2305_BF_R)

#Define other geometry data
#BF_L_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2305, 7] <- 2647.118-min(BF_xsxn_2305$elevation_m)

#BF_L_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2305, 8] <- 29.7180-xsxn_2305_BF_L

#BF_R_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2305, 9] <- 2647.176-min(BF_xsxn_2305$elevation_m)

#BF_R_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2305, 10] <- xsxn_2305_BF_R-31.0896

#BF_base_length
xsxn_LOI2[xsxn_LOI2$xsxn_id==2305, 13] <- 31.0896-29.7180

#xsxn 2327
# define end points of bankfull channel
xsxn_2327_BF_L <- 30.6324
xsxn_2327_BF_R <- 43.8912
xsxn_LOI2[xsxn_LOI2$xsxn_id==2327, 5:6] <- as.list(c(xsxn_2327_BF_L, xsxn_2327_BF_R))

#filter points for easier calculations
BF_xsxn_2327 <- xsxn_LOI2_all %>%
  filter(xsxn_id == 2327) %>% 
  filter(location_m >= xsxn_2327_BF_L & location_m <= xsxn_2327_BF_R)

#Define other geometry data
#BF_L_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2327, 7] <- 2642.217-min(BF_xsxn_2327$elevation_m)

#BF_L_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2327, 8] <- 35.6616-xsxn_2327_BF_L

#BF_R_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2327, 9] <- 2642.349-min(BF_xsxn_2327$elevation_m)

#BF_R_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2327, 10] <- xsxn_2327_BF_R-38.4048

#BF_base_length
xsxn_LOI2[xsxn_LOI2$xsxn_id==2327, 13] <- 38.4048-35.6616

#xsxn 2396
# define end points of bankfull channel
xsxn_2396_BF_L <- 25.1460
xsxn_2396_BF_R <- 33.8328
xsxn_LOI2[xsxn_LOI2$xsxn_id==2396, 5:6] <- as.list(c(xsxn_2396_BF_L, xsxn_2396_BF_R))

#filter points for easier calculations
BF_xsxn_2396 <- xsxn_LOI2_all %>%
  filter(xsxn_id == 2396) %>% 
  filter(location_m >= xsxn_2396_BF_L & location_m <= xsxn_2396_BF_R)

#Define other geometry data
#BF_L_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2396, 7] <- 2636.015-min(BF_xsxn_2396$elevation_m)

#BF_L_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2396, 8] <- 28.8036-xsxn_2396_BF_L

#BF_R_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2396, 9] <- 2635.722-min(BF_xsxn_2396$elevation_m)

#BF_R_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2396, 10] <- xsxn_2396_BF_R-32.4612

#BF_base_length
xsxn_LOI2[xsxn_LOI2$xsxn_id==2396, 13] <- 32.4612-28.8036

#xsxn 2412
# define end points of bankfull channel
xsxn_2412_BF_L <- 25.1460
xsxn_2412_BF_R <- 34.2900
xsxn_LOI2[xsxn_LOI2$xsxn_id==2412, 5:6] <- as.list(c(xsxn_2412_BF_L, xsxn_2412_BF_R))

#filter points for easier calculations
BF_xsxn_2412 <- xsxn_LOI2_all %>%
  filter(xsxn_id == 2412) %>% 
  filter(location_m >= xsxn_2412_BF_L & location_m <= xsxn_2412_BF_R)

#Define other geometry data
#BF_L_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2412, 7] <- 2623.467-min(BF_xsxn_2412$elevation_m)

#BF_L_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2412, 8] <- 27.4320-xsxn_2412_BF_L

#BF_R_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2412, 9] <- 2623.485-min(BF_xsxn_2412$elevation_m)

#BF_R_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2412, 10] <- xsxn_2412_BF_R-31.5468

#BF_base_length
xsxn_LOI2[xsxn_LOI2$xsxn_id==2412, 13] <- 31.5468-27.4320

#xsxn 2499
# define end points of bankfull channel
xsxn_2499_BF_L <- 32.4612
xsxn_2499_BF_R <- 34.7472
xsxn_LOI2[xsxn_LOI2$xsxn_id==2499, 5:6] <- as.list(c(xsxn_2499_BF_L, xsxn_2499_BF_R))

#filter points for easier calculations
BF_xsxn_2499 <- xsxn_LOI2_all %>%
  filter(xsxn_id == 2499) %>% 
  filter(location_m >= xsxn_2499_BF_L & location_m <= xsxn_2499_BF_R)

#Define other geometry data
#BF_L_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2499, 7] <- 2608.661-min(BF_xsxn_2499$elevation_m)

#BF_L_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2499, 8] <- 32.9184-xsxn_2499_BF_L

#BF_R_height_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2499, 9] <- 2608.684-min(BF_xsxn_2499$elevation_m)

#BF_R_width_m
xsxn_LOI2[xsxn_LOI2$xsxn_id==2499, 10] <- xsxn_2499_BF_R-33.3756

#BF_base_length
xsxn_LOI2[xsxn_LOI2$xsxn_id==2499, 13] <- 33.3756-32.9184

# LOI2: Calculate bankfull flow -------------------------------------------------

#When all data is identified for each cross-section, calculate:
#BF_height: choose the minimum of the side heights 
xsxn_LOI2$BF_h_m <- pmin(xsxn_LOI2$BF_L_h_m, xsxn_LOI2$BF_R_h_m)

#BF_top_length
xsxn_LOI2$xsxn_BF_top_length_m <- xsxn_LOI2$BF_R_m - xsxn_LOI2$BF_L_m

#BF_L_length
xsxn_LOI2$xsxn_BF_L_length_m <- sqrt((xsxn_LOI2$BF_L_h_m)^2+(xsxn_LOI2$BF_L_w_m)^2)

#BF_R_length
xsxn_LOI2$xsxn_BF_R_length_m <- sqrt((xsxn_LOI2$BF_R_h_m)^2+(xsxn_LOI2$BF_R_w_m)^2)

# Wetted perimeter
xsxn_LOI2$R_hyd_rad <- xsxn_LOI2$xsxn_BF_L_length_m+xsxn_LOI2$xsxn_BF_R_length_m+xsxn_LOI2$xsxn_BF_base_length_m

#Bankfull area
xsxn_LOI2$Area <- (1/2)*(xsxn_LOI2$xsxn_BF_top_length_m+xsxn_LOI2$xsxn_BF_base_length_m)*xsxn_LOI2$BF_h_m

#Bankfull flow
# Calculate flow (Q) using Manning's equation: 
#      If using U.S. units: Q = (1.49/n)*A*R^(2/3)*S^(1/2)
#      If using S.I. units: Q = (1.00/n)*A*R^(2/3)*S^(1/2)
#Units are all metric, so will use the SI version of Manning's equation; the 1.00 is the k conversion factor to calculate flow in cfs.

xsxn_LOI2$BF_flow_cfs <- (1.00/xsxn_LOI2$Manning_n)*xsxn_LOI2$Area*xsxn_LOI2$R_hyd_rad^(2/3)*xsxn_LOI2$slope^(1/2)

# LOI1: Identify bankfull geometry ----------------------------------------------
