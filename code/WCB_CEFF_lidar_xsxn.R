
# Code description --------------------------------------------------------

# This code was developed to extract cross-section digital elevation data from the LiDAR dataset of the Little Shasta River (part of a total Shasta Valley LiDAR survey). These cross-sections will be analyzed as part of the CEFF-Little Shasta research for Section B: determining the extent to which channel degradation might prevent a functional flow regime that resembles the predicted metrics from successfully supporting the desired ecosystem function.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf) #used to load and read spatial data
library(mapview)
library(mapedit) #used to create a layer of all cross-sections
library(sp) #required for raster package
library(raster) #used to convert sf layer to raster, then extract DEM data from that
library(elevatr) #used to extract DEM data

#in the future, the lidR package can be used to directly read the las files. The work to merge the individual LiDAR tiles for the Little Shasta into a single layer was already completed with ArcMap 10.5.


# Load data ---------------------------------------------------------------

#Coordinate systems are not consistent between files. Need to convert from NAD83 (EPSG: 4269) to UTM (EPSG: 32610).

LSR_LiDAR_boundary <- st_read("data/GIS/LSR_LiDAR_boundary.shp")
LSR_streamline <- st_read("data/GIS/Little_Shasta_waterways_1.shp")

LSR_LiDAR_boundary_UTM <- st_transform(LSR_LiDAR_boundary, crs = 32610)

#check that project transformation worked:
LSR_LiDAR_boundary_UTM

#Looks good! Repeat for streamline
LSR_streamline_UTM <- st_transform(LSR_streamline, crs = 32610)


# Plot and view
ggplot() +
  geom_sf(data = LSR_LiDAR_boundary_UTM) +
  geom_sf(data = LSR_streamline_UTM) +
  coord_sf()

# Wrangle spatial data ----------------------------------------------------
#Need to drop M from UTM geometries

LSR_LiDAR_boundary_clip <- st_zm(LSR_LiDAR_boundary_UTM)

LSR_streamline_clip <- st_zm(LSR_streamline_UTM)

LSR_clipped <- st_intersection(LSR_LiDAR_boundary_clip,LSR_streamline_clip)

#plot and see if clipped streamline is correct
ggplot() +
  geom_sf(data = LSR_LiDAR_boundary_UTM) +
  geom_sf(data = LSR_clipped) +
  coord_sf()

#Combine stream segments into one. Use clipped streamline to create a buffered area from which to draw cross-sections and extract DEM data

LSR_buffer <- LSR_clipped %>% 
  st_combine() %>% 
  st_buffer(dist = 500)

#plot and see if buffer was successfully made
ggplot() +
  geom_sf(data = LSR_LiDAR_boundary_clip) +
  geom_sf(data = LSR_buffer) +
  coord_sf()

# Nice! Now make a shp file of cross-sections

# SKIP - Draw and save cross-sections --------------------------------------------
#These lines commented out so that the dataframe is not accidentally overwritten. Commented code saved for posterity.

#LSR_xsxns <- mapview(LSR_buffer) %>% 
#  editMap()

# LSR_xsxns_object <- LSR_xsxns$finished
# 
# # LSR_xsxns_sf <- LSR_xsxns_object %>% 
# #   select(geometry)
# 
# #view buffer and cross-sections
# mapview(LSR_buffer) +
#   mapview(LSR_xsxns_object)
# 
# #Save object of cross-sections
# save(LSR_xsxns_object, file = "data/GIS/LSR_xsxns.rda")


# Clip raster -------------------------------------------------------------

load("data/GIS/LSR_xsxns.rda")

#Tif of Little Shasta lidar is saved in X:\ShastaRiver\SpatialData\ImageFiles\Little_Shasta\little_shasta_lidar_dsm1.tif

library(tiff)

LSR_tiff <- "X:\ShastaRiver\SpatialData\ImageFiles\Little_Shasta\little_shasta_lidar_dsm1.tif"

imported_raster=raster('X:\ShastaRiver\SpatialData\ImageFiles\Little_Shasta\little_shasta_lidar_dsm1.tif')
