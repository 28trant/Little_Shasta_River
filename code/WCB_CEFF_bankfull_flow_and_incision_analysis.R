
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
xsxn_LOI1$LOI_id <- "LOI_1"
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
xsxn_LOI3$LOI_id <- "LOI_3"
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
#Units are all metric, so will use the SI version of Manning's equation; result is multiplied by 35.315 to convert units to cfs.

xsxn_LOI3$BF_flow_cfs <- (1.00/xsxn_LOI3$Manning_n)*xsxn_LOI3$Area*xsxn_LOI3$R_hyd_rad^(2/3)*xsxn_LOI3$slope^(1/2)*35.315

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
#Units are all metric, so will use the SI version of Manning's equation; result is multiplied by 35.315 to convert units to cfs.

xsxn_LOI2$BF_flow_cfs <- (1.00/xsxn_LOI2$Manning_n)*xsxn_LOI2$Area*xsxn_LOI2$R_hyd_rad^(2/3)*xsxn_LOI2$slope^(1/2)*35.315

# LOI1: Identify bankfull geometry ----------------------------------------------

# Identify bankfull segment by comparing the plotly line with the map to confirm channel location. Bankfull geometry includes hydraulic radius (wetted perimeter) and area variables. Wetted perimeter includes length of bankfull channel (3 sides); may need to use Pythagoreum theorum (c^2 = a^2 + b^2) to calculate diagonal bank distance. Area likely to be area of trapezoid: (1/2)*(a+b)*h where b = length of base and a = length of top side.

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

#xsxn 2999
# define end points and elevation of bankfull channel
xsxn_2999_BF_L <- 12.3444
xsxn_2999_BF_R <- 26.0604
xsxn_2999_BF_L_elev <- 2544.785
xsxn_2999_BF_R_elev <- 2544.745

#Fill in table identifying end points of bankfull cross-secion
xsxn_LOI1[xsxn_LOI1$xsxn_id==2999, 5:6] <- as.list(c(xsxn_2999_BF_L, xsxn_2999_BF_R))

# define end points and elevation of channel base
xsxn_2999_base_L <- 19.2024
xsxn_2999_base_L_elev <- 2542.151

xsxn_2999_base_R <- 20.5740 
xsxn_2999_base_R_elev <-  2542.154

#filter points for easier calculations
BF_xsxn_2999 <- xsxn_LOI1_all %>%
  filter(xsxn_id == 2999) %>% 
  filter(location_m >= xsxn_2999_BF_L & location_m <= xsxn_2999_BF_R)

#Define other geometry data
#BF_L_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==2999, 7] <- xsxn_2999_BF_L_elev-min(BF_xsxn_2999$elevation_m)

#BF_L_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==2999, 8] <- xsxn_2999_base_L-xsxn_2999_BF_L

#BF_R_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==2999, 9] <- xsxn_2999_BF_R_elev-min(BF_xsxn_2999$elevation_m)

#BF_R_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==2999, 10] <- xsxn_2999_BF_R-xsxn_2999_base_R

#BF_base_length
xsxn_LOI1[xsxn_LOI1$xsxn_id==2999, 13] <- xsxn_2999_base_R-xsxn_2999_base_L

#xsxn 3351
# define end points and elevation of bankfull channel
xsxn_3351_BF_L <- 22.8600
xsxn_3351_BF_L_elev <- 2540.190

xsxn_3351_BF_R <- 27.4320
xsxn_3351_BF_R_elev <- 2540.144

#Fill in table identifying end points of bankfull cross-secion
xsxn_LOI1[xsxn_LOI1$xsxn_id==3351, 5:6] <- as.list(c(xsxn_3351_BF_L, xsxn_3351_BF_R))

# define end points and elevation of channel base
xsxn_3351_base_L <- 25.1460
xsxn_3351_base_L_elev <- 2537.890

xsxn_3351_base_R <-  25.6032
xsxn_3351_base_R_elev <- 2537.887 

#filter points for easier calculations
BF_xsxn_3351 <- xsxn_LOI1_all %>%
  filter(xsxn_id == 3351) %>% 
  filter(location_m >= xsxn_3351_BF_L & location_m <= xsxn_3351_BF_R)

#Define other geometry data
#BF_L_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3351, 7] <- xsxn_3351_BF_L_elev-min(BF_xsxn_3351$elevation_m)

#BF_L_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3351, 8] <- xsxn_3351_base_L-xsxn_3351_BF_L

#BF_R_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3351, 9] <- xsxn_3351_BF_R_elev-min(BF_xsxn_3351$elevation_m)

#BF_R_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3351, 10] <- xsxn_3351_BF_R-xsxn_3351_base_R

#BF_base_length
xsxn_LOI1[xsxn_LOI1$xsxn_id==3351, 13] <- xsxn_3351_base_R-xsxn_3351_base_L

#xsxn 3371
# define end points and elevation of bankfull channel
xsxn_3371_BF_L <- 41.1480
xsxn_3371_BF_L_elev <- 2534.698

xsxn_3371_BF_R <- 48.9204
xsxn_3371_BF_R_elev <- 2534.498

#Fill in table identifying end points of bankfull cross-secion
xsxn_LOI1[xsxn_LOI1$xsxn_id==3371, 5:6] <- as.list(c(xsxn_3371_BF_L, xsxn_3371_BF_R))

# define end points and elevation of channel base
xsxn_3371_base_L <- 42.5196
xsxn_3371_base_L_elev <- 2533.221

xsxn_3371_base_R <- 47.5488
xsxn_3371_base_R_elev <- 2533.514

#filter points for easier calculations
BF_xsxn_3371 <- xsxn_LOI1_all %>%
  filter(xsxn_id == 3371) %>% 
  filter(location_m >= xsxn_3371_BF_L & location_m <= xsxn_3371_BF_R)

#Define other geometry data
#BF_L_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3371, 7] <- xsxn_3371_BF_L_elev-min(BF_xsxn_3371$elevation_m)

#BF_L_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3371, 8] <- xsxn_3371_base_L-xsxn_3371_BF_L

#BF_R_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3371, 9] <- xsxn_3371_BF_R_elev-min(BF_xsxn_3371$elevation_m)

#BF_R_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3371, 10] <- xsxn_3371_BF_R-xsxn_3371_base_R

#BF_base_length
xsxn_LOI1[xsxn_LOI1$xsxn_id==3371, 13] <- xsxn_3371_base_R-xsxn_3371_base_L

#xsxn 3408
# define end points and elevation of bankfull channel
xsxn_3408_BF_L <- 16.4592
xsxn_3408_BF_L_elev <- 2527.072

xsxn_3408_BF_R <- 27.4320
xsxn_3408_BF_R_elev <- 2526.995

#Fill in table identifying end points of bankfull cross-secion
xsxn_LOI1[xsxn_LOI1$xsxn_id==3408, 5:6] <- as.list(c(xsxn_3408_BF_L, xsxn_3408_BF_R))

# define end points and elevation of channel base
xsxn_3408_base_L <- 20.1168
xsxn_3408_base_L_elev <- 2524.666

xsxn_3408_base_R <- 24.6888
xsxn_3408_base_R_elev <-  2524.855

#filter points for easier calculations
BF_xsxn_3408 <- xsxn_LOI1_all %>%
  filter(xsxn_id == 3408) %>% 
  filter(location_m >= xsxn_3408_BF_L & location_m <= xsxn_3408_BF_R)

#Define other geometry data
#BF_L_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3408, 7] <- xsxn_3408_BF_L_elev-min(BF_xsxn_3408$elevation_m)

#BF_L_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3408, 8] <- xsxn_3408_base_L-xsxn_3408_BF_L

#BF_R_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3408, 9] <- xsxn_3408_BF_R_elev-min(BF_xsxn_3408$elevation_m)

#BF_R_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3408, 10] <- xsxn_3408_BF_R-xsxn_3408_base_R

#BF_base_length
xsxn_LOI1[xsxn_LOI1$xsxn_id==3408, 13] <- xsxn_3408_base_R-xsxn_3408_base_L

#xsxn 3423
# define end points and elevation of bankfull channel
xsxn_3423_BF_L <- 25.1460
xsxn_3423_BF_L_elev <- 2523.169

xsxn_3423_BF_R <- 37.0332
xsxn_3423_BF_R_elev <- 2523.265

# define end points and elevation of channel base
xsxn_3423_base_L <- 27.8892
xsxn_3423_base_L_elev <- 2522.051

xsxn_3423_base_R <- 31.5468
xsxn_3423_base_R_elev <- 2522.071

#filter points for easier calculations
BF_xsxn_3423 <- xsxn_LOI1_all %>%
  filter(xsxn_id == 3423) %>% 
  filter(location_m >= xsxn_3423_BF_L & location_m <= xsxn_3423_BF_R)

#Fill in table identifying end points of bankfull cross-secion
xsxn_LOI1[xsxn_LOI1$xsxn_id==3423, 5:6] <- as.list(c(xsxn_3423_BF_L, xsxn_3423_BF_R))

#Define other geometry data
#BF_L_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3423, 7] <- xsxn_3423_BF_L_elev-min(BF_xsxn_3423$elevation_m)

#BF_L_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3423, 8] <- xsxn_3423_base_L-xsxn_3423_BF_L

#BF_R_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3423, 9] <- xsxn_3423_BF_R_elev-min(BF_xsxn_3423$elevation_m)

#BF_R_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3423, 10] <- xsxn_3423_BF_R-xsxn_3423_base_R

#BF_base_length
xsxn_LOI1[xsxn_LOI1$xsxn_id==3423, 13] <- xsxn_3423_base_R-xsxn_3423_base_L

#xsxn 3503
# define end points and elevation of bankfull channel
xsxn_3503_BF_L <- 23.7744
xsxn_3503_BF_L_elev <- 2517.512

xsxn_3503_BF_R <- 31.0896
xsxn_3503_BF_R_elev <- 2517.474

# define end points and elevation of channel base
xsxn_3503_base_L <- 26.9748
xsxn_3503_base_L_elev <- 2516.659

xsxn_3503_base_R <- 28.3464
xsxn_3503_base_R_elev <- 2516.630

#filter points for easier calculations
BF_xsxn_3503 <- xsxn_LOI1_all %>%
  filter(xsxn_id == 3503) %>% 
  filter(location_m >= xsxn_3503_BF_L & location_m <= xsxn_3503_BF_R)

#Fill in table identifying end points of bankfull cross-secion
xsxn_LOI1[xsxn_LOI1$xsxn_id==3503, 5:6] <- as.list(c(xsxn_3503_BF_L, xsxn_3503_BF_R))

#Define other geometry data
#BF_L_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3503, 7] <- xsxn_3503_BF_L_elev-min(BF_xsxn_3503$elevation_m)

#BF_L_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3503, 8] <- xsxn_3503_base_L-xsxn_3503_BF_L

#BF_R_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3503, 9] <- xsxn_3503_BF_R_elev-min(BF_xsxn_3503$elevation_m)

#BF_R_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3503, 10] <- xsxn_3503_BF_R-xsxn_3503_base_R

#BF_base_length
xsxn_LOI1[xsxn_LOI1$xsxn_id==3503, 13] <- xsxn_3503_base_R-xsxn_3503_base_L

#xsxn 3519
# define end points and elevation of bankfull channel
xsxn_3519_BF_L <- 33.3756
xsxn_3519_BF_L_elev <- 2513.904

xsxn_3519_BF_R <- 42.0624
xsxn_3519_BF_R_elev <- 2513.986

# define end points and elevation of channel base
xsxn_3519_base_L <- 36.1188
xsxn_3519_base_L_elev <- 2513.109

xsxn_3519_base_R <- 40.2336
xsxn_3519_base_R_elev <- 2513.094

#filter points for easier calculations
BF_xsxn_3519 <- xsxn_LOI1_all %>%
  filter(xsxn_id == 3519) %>% 
  filter(location_m >= xsxn_3519_BF_L & location_m <= xsxn_3519_BF_R)

#Fill in table identifying end points of bankfull cross-secion
xsxn_LOI1[xsxn_LOI1$xsxn_id==3519, 5:6] <- as.list(c(xsxn_3519_BF_L, xsxn_3519_BF_R))

#Define other geometry data
#BF_L_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3519, 7] <- xsxn_3519_BF_L_elev-min(BF_xsxn_3519$elevation_m)

#BF_L_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3519, 8] <- xsxn_3519_base_L-xsxn_3519_BF_L

#BF_R_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3519, 9] <- xsxn_3519_BF_R_elev-min(BF_xsxn_3519$elevation_m)

#BF_R_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3519, 10] <- xsxn_3519_BF_R-xsxn_3519_base_R

#BF_base_length
xsxn_LOI1[xsxn_LOI1$xsxn_id==3519, 13] <- xsxn_3519_base_R-xsxn_3519_base_L

#xsxn 3620
# define end points and elevation of bankfull channel
xsxn_3620_BF_L <- 28.8036
xsxn_3620_BF_L_elev <- 2505.208

xsxn_3620_BF_R <- 38.4048
xsxn_3620_BF_R_elev <- 2505.304

# define end points and elevation of channel base
xsxn_3620_base_L <- 30.6324
xsxn_3620_base_L_elev <- 2503.353

xsxn_3620_base_R <- 34.7472
xsxn_3620_base_R_elev <- 2503.459

#filter points for easier calculations
BF_xsxn_3620 <- xsxn_LOI1_all %>%
  filter(xsxn_id == 3620) %>% 
  filter(location_m >= xsxn_3620_BF_L & location_m <= xsxn_3620_BF_R)

#Fill in table identifying end points of bankfull cross-secion
xsxn_LOI1[xsxn_LOI1$xsxn_id==3620, 5:6] <- as.list(c(xsxn_3620_BF_L, xsxn_3620_BF_R))

#Define other geometry data
#BF_L_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3620, 7] <- xsxn_3620_BF_L_elev-min(BF_xsxn_3620$elevation_m)

#BF_L_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3620, 8] <- xsxn_3620_base_L-xsxn_3620_BF_L

#BF_R_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3620, 9] <- xsxn_3620_BF_R_elev-min(BF_xsxn_3620$elevation_m)

#BF_R_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3620, 10] <- xsxn_3620_BF_R-xsxn_3620_base_R

#BF_base_length
xsxn_LOI1[xsxn_LOI1$xsxn_id==3620, 13] <- xsxn_3620_base_R-xsxn_3620_base_L

#xsxn 3634
# define end points and elevation of bankfull channel
xsxn_3634_BF_L <- 19.2024
xsxn_3634_BF_L_elev <- 2499.090

xsxn_3634_BF_R <- 25.6032
xsxn_3634_BF_R_elev <- 2499.050

# define end points and elevation of channel base
xsxn_3634_base_L <- 20.5740
xsxn_3634_base_L_elev <- 2498.410

xsxn_3634_base_R <- 24.2316
xsxn_3634_base_R_elev <- 2498.446

#filter points for easier calculations
BF_xsxn_3634 <- xsxn_LOI1_all %>%
  filter(xsxn_id == 3634) %>% 
  filter(location_m >= xsxn_3634_BF_L & location_m <= xsxn_3634_BF_R)

#Fill in table identifying end points of bankfull cross-secion
xsxn_LOI1[xsxn_LOI1$xsxn_id==3634, 5:6] <- as.list(c(xsxn_3634_BF_L, xsxn_3634_BF_R))

#Define other geometry data
#BF_L_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3634, 7] <- xsxn_3634_BF_L_elev-min(BF_xsxn_3634$elevation_m)

#BF_L_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3634, 8] <- xsxn_3634_base_L-xsxn_3634_BF_L

#BF_R_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3634, 9] <- xsxn_3634_BF_R_elev-min(BF_xsxn_3634$elevation_m)

#BF_R_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3634, 10] <- xsxn_3634_BF_R-xsxn_3634_base_R

#BF_base_length
xsxn_LOI1[xsxn_LOI1$xsxn_id==3634, 13] <- xsxn_3634_base_R-xsxn_3634_base_L

#xsxn 3691
# define end points and elevation of bankfull channel
xsxn_3691_BF_L <- 18.7452
xsxn_3691_BF_L_elev <- 2494.784

xsxn_3691_BF_R <- 25.1460
xsxn_3691_BF_R_elev <- 2494.815

# define end points and elevation of channel base
xsxn_3691_base_L <- 19.6596
xsxn_3691_base_L_elev <- 2494.360

xsxn_3691_base_R <- 21.4884
xsxn_3691_base_R_elev <- 2494.489

#filter points for easier calculations
BF_xsxn_3691 <- xsxn_LOI1_all %>%
  filter(xsxn_id == 3691) %>% 
  filter(location_m >= xsxn_3691_BF_L & location_m <= xsxn_3691_BF_R)

#Fill in table identifying end points of bankfull cross-secion
xsxn_LOI1[xsxn_LOI1$xsxn_id==3691, 5:6] <- as.list(c(xsxn_3691_BF_L, xsxn_3691_BF_R))

#Define other geometry data
#BF_L_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3691, 7] <- xsxn_3691_BF_L_elev-min(BF_xsxn_3691$elevation_m)

#BF_L_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3691, 8] <- xsxn_3691_base_L-xsxn_3691_BF_L

#BF_R_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3691, 9] <- xsxn_3691_BF_R_elev-min(BF_xsxn_3691$elevation_m)

#BF_R_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3691, 10] <- xsxn_3691_BF_R-xsxn_3691_base_R

#BF_base_length
xsxn_LOI1[xsxn_LOI1$xsxn_id==3691, 13] <- xsxn_3691_base_R-xsxn_3691_base_L

#xsxn 3705
# define end points and elevation of bankfull channel
xsxn_3705_BF_L <- 19.2024
xsxn_3705_BF_L_elev <- 2490.626

xsxn_3705_BF_R <- 37.9476
xsxn_3705_BF_R_elev <- 2490.587

# define end points and elevation of channel base
xsxn_3705_base_L <- 28.3464
xsxn_3705_base_L_elev <- 2487.969

xsxn_3705_base_R <- 31.0896
xsxn_3705_base_R_elev <- 2487.987

#filter points for easier calculations
BF_xsxn_3705 <- xsxn_LOI1_all %>%
  filter(xsxn_id == 3705) %>% 
  filter(location_m >= xsxn_3705_BF_L & location_m <= xsxn_3705_BF_R)

#Fill in table identifying end points of bankfull cross-secion
xsxn_LOI1[xsxn_LOI1$xsxn_id==3705, 5:6] <- as.list(c(xsxn_3705_BF_L, xsxn_3705_BF_R))

#Define other geometry data
#BF_L_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3705, 7] <- xsxn_3705_BF_L_elev-min(BF_xsxn_3705$elevation_m)

#BF_L_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3705, 8] <- xsxn_3705_base_L-xsxn_3705_BF_L

#BF_R_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3705, 9] <- xsxn_3705_BF_R_elev-min(BF_xsxn_3705$elevation_m)

#BF_R_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3705, 10] <- xsxn_3705_BF_R-xsxn_3705_base_R

#BF_base_length
xsxn_LOI1[xsxn_LOI1$xsxn_id==3705, 13] <- xsxn_3705_base_R-xsxn_3705_base_L

#xsxn 3721
# define end points and elevation of bankfull channel
xsxn_3721_BF_L <- 35.2044
xsxn_3721_BF_L_elev <- 2487.108

xsxn_3721_BF_R <- 45.7200
xsxn_3721_BF_R_elev <- 2486.976

# define end points and elevation of channel base
xsxn_3721_base_L <- 38.4048
xsxn_3721_base_L_elev <- 2484.212

xsxn_3721_base_R <- 42.0624
xsxn_3721_base_R_elev <- 2484.331

#filter points for easier calculations
BF_xsxn_3721 <- xsxn_LOI1_all %>%
  filter(xsxn_id == 3721) %>% 
  filter(location_m >= xsxn_3721_BF_L & location_m <= xsxn_3721_BF_R)

#Fill in table identifying end points of bankfull cross-secion
xsxn_LOI1[xsxn_LOI1$xsxn_id==3721, 5:6] <- as.list(c(xsxn_3721_BF_L, xsxn_3721_BF_R))

#Define other geometry data
#BF_L_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3721, 7] <- xsxn_3721_BF_L_elev-min(BF_xsxn_3721$elevation_m)

#BF_L_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3721, 8] <- xsxn_3721_base_L-xsxn_3721_BF_L

#BF_R_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3721, 9] <- xsxn_3721_BF_R_elev-min(BF_xsxn_3721$elevation_m)

#BF_R_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3721, 10] <- xsxn_3721_BF_R-xsxn_3721_base_R

#BF_base_length
xsxn_LOI1[xsxn_LOI1$xsxn_id==3721, 13] <- xsxn_3721_base_R-xsxn_3721_base_L

#xsxn 3800
# define end points and elevation of bankfull channel
xsxn_3800_BF_L <- 20.1168
xsxn_3800_BF_L_elev <- 2480.086

xsxn_3800_BF_R <- 22.4028
xsxn_3800_BF_R_elev <- 2480.307

# define end points and elevation of channel base
xsxn_3800_base_L <- 21.0312
xsxn_3800_base_L_elev <- 2479.750

xsxn_3800_base_R <- 21.9456
xsxn_3800_base_R_elev <- 2479.877

#filter points for easier calculations
BF_xsxn_3800 <- xsxn_LOI1_all %>%
  filter(xsxn_id == 3800) %>% 
  filter(location_m >= xsxn_3800_BF_L & location_m <= xsxn_3800_BF_R)

#Fill in table identifying end points of bankfull cross-secion
xsxn_LOI1[xsxn_LOI1$xsxn_id==3800, 5:6] <- as.list(c(xsxn_3800_BF_L, xsxn_3800_BF_R))

#Define other geometry data
#BF_L_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3800, 7] <- xsxn_3800_BF_L_elev-min(BF_xsxn_3800$elevation_m)

#BF_L_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3800, 8] <- xsxn_3800_base_L-xsxn_3800_BF_L

#BF_R_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3800, 9] <- xsxn_3800_BF_R_elev-min(BF_xsxn_3800$elevation_m)

#BF_R_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3800, 10] <- xsxn_3800_BF_R-xsxn_3800_base_R

#BF_base_length
xsxn_LOI1[xsxn_LOI1$xsxn_id==3800, 13] <- xsxn_3800_base_R-xsxn_3800_base_L

#xsxn 3822
# define end points and elevation of bankfull channel
xsxn_3822_BF_L <- 21.0312
xsxn_3822_BF_L_elev <- 2480.408

xsxn_3822_BF_R <- 31.5468
xsxn_3822_BF_R_elev <- 2480.612

# define end points and elevation of channel base
xsxn_3822_base_L <- 25.1460
xsxn_3822_base_L_elev <- 2477.951

xsxn_3822_base_R <- 26.9748
xsxn_3822_base_R_elev <- 2478.021

#filter points for easier calculations
BF_xsxn_3822 <- xsxn_LOI1_all %>%
  filter(xsxn_id == 3822) %>% 
  filter(location_m >= xsxn_3822_BF_L & location_m <= xsxn_3822_BF_R)

#Fill in table identifying end points of bankfull cross-secion
xsxn_LOI1[xsxn_LOI1$xsxn_id==3822, 5:6] <- as.list(c(xsxn_3822_BF_L, xsxn_3822_BF_R))

#Define other geometry data
#BF_L_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3822, 7] <- xsxn_3822_BF_L_elev-min(BF_xsxn_3822$elevation_m)

#BF_L_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3822, 8] <- xsxn_3822_base_L-xsxn_3822_BF_L

#BF_R_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3822, 9] <- xsxn_3822_BF_R_elev-min(BF_xsxn_3822$elevation_m)

#BF_R_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3822, 10] <- xsxn_3822_BF_R-xsxn_3822_base_R

#BF_base_length
xsxn_LOI1[xsxn_LOI1$xsxn_id==3822, 13] <- xsxn_3822_base_R-xsxn_3822_base_L

#xsxn 3836
# define end points and elevation of bankfull channel
xsxn_3836_BF_L <- 21.4884
xsxn_3836_BF_L_elev <- 2478.258

xsxn_3836_BF_R <- 37.9476
xsxn_3836_BF_R_elev <- 2478.116

# define end points and elevation of channel base
xsxn_3836_base_L <- 25.6032
xsxn_3836_base_L_elev <- 2477.025

xsxn_3836_base_R <- 33.3756
xsxn_3836_base_R_elev <- 2477.132

#filter points for easier calculations
BF_xsxn_3836 <- xsxn_LOI1_all %>%
  filter(xsxn_id == 3836) %>% 
  filter(location_m >= xsxn_3836_BF_L & location_m <= xsxn_3836_BF_R)

#Fill in table identifying end points of bankfull cross-secion
xsxn_LOI1[xsxn_LOI1$xsxn_id==3836, 5:6] <- as.list(c(xsxn_3836_BF_L, xsxn_3836_BF_R))

#Define other geometry data
#BF_L_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3836, 7] <- xsxn_3836_BF_L_elev-min(BF_xsxn_3836$elevation_m)

#BF_L_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3836, 8] <- xsxn_3836_base_L-xsxn_3836_BF_L

#BF_R_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3836, 9] <- xsxn_3836_BF_R_elev-min(BF_xsxn_3836$elevation_m)

#BF_R_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3836, 10] <- xsxn_3836_BF_R-xsxn_3836_base_R

#BF_base_length
xsxn_LOI1[xsxn_LOI1$xsxn_id==3836, 13] <- xsxn_3836_base_R-xsxn_3836_base_L

#xsxn 3860
# define end points and elevation of bankfull channel
xsxn_3860_BF_L <- 14.1732
xsxn_3860_BF_L_elev <- 2472.922

xsxn_3860_BF_R <- 17.3736
xsxn_3860_BF_R_elev <- 2472.986

# define end points and elevation of channel base
xsxn_3860_base_L <- 15.5448
xsxn_3860_base_L_elev <- 2472.626

xsxn_3860_base_R <- 16.9164
xsxn_3860_base_R_elev <- 2472.672

#filter points for easier calculations
BF_xsxn_3860 <- xsxn_LOI1_all %>%
  filter(xsxn_id == 3860) %>% 
  filter(location_m >= xsxn_3860_BF_L & location_m <= xsxn_3860_BF_R)

#Fill in table identifying end points of bankfull cross-secion
xsxn_LOI1[xsxn_LOI1$xsxn_id==3860, 5:6] <- as.list(c(xsxn_3860_BF_L, xsxn_3860_BF_R))

#Define other geometry data
#BF_L_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3860, 7] <- xsxn_3860_BF_L_elev-min(BF_xsxn_3860$elevation_m)

#BF_L_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3860, 8] <- xsxn_3860_base_L-xsxn_3860_BF_L

#BF_R_height_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3860, 9] <- xsxn_3860_BF_R_elev-min(BF_xsxn_3860$elevation_m)

#BF_R_width_m
xsxn_LOI1[xsxn_LOI1$xsxn_id==3860, 10] <- xsxn_3860_BF_R-xsxn_3860_base_R

#BF_base_length
xsxn_LOI1[xsxn_LOI1$xsxn_id==3860, 13] <- xsxn_3860_base_R-xsxn_3860_base_L

# LOI1: Calculate bankfull flow -------------------------------------------------

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
#Units are all metric, so will use the SI version of Manning's equation; result is multiplied by 35.315 to convert units to cfs.

xsxn_LOI1$BF_flow_cfs <- (1.49/xsxn_LOI1$Manning_n)*xsxn_LOI1$Area*xsxn_LOI1$R_hyd_rad^(2/3)*xsxn_LOI1$slope^(1/2)

# Compile and save results ------------------------------------------------

LOI_all_xsxns_and_bankfull_flow <- rbind(xsxn_LOI1, xsxn_LOI2)
LOI_all_xsxns_and_bankfull_flow <- rbind(LOI_all_xsxns_and_bankfull_flow, xsxn_LOI3)

