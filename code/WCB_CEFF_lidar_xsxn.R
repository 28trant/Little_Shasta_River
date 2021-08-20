
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
library(rgdal)

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


# Extract xsxn elevations from raster -------------------------------------------------------------

load("data/GIS/LSR_xsxns.rda")

#Plot to make sure cross-sections exist

ggplot() +
  geom_sf(data = LSR_LiDAR_boundary_clip) +
  geom_sf(data = LSR_buffer) +
  geom_sf(data = LSR_xsxns_object) +
  coord_sf()

#Tif of Little Shasta lidar is saved in X:\ShastaRiver\SpatialData\ImageFiles\Little_Shasta\little_shasta_lidar_dsm1.tif

#library(tiff)

LSR_path <- "X:/ShastaRiver/SpatialData/ImageFiles/Little_Shasta/little_shasta_lidar_dsm1.tif"

LSR_tiff=raster(LSR_path)

#Add original projection to raster
LSR_nad83 <- "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"

#check projection of raster
crs(LSR_tiff)

#Confirm projection of buffer layer
st_crs(LSR_buffer)

#convert tiff to UTM projection - this takes a REALLY long time, and shouldn't be re-run, if possible. Commented out this and the extraction lines to avoid accidentally running them.
# LSR_tiff_UTM <- projectRaster(LSR_tiff, crs = "+init=EPSG:32610")

#confirm new projection
# crs(LSR_tiff_UTM)

#save raster with UTM projection
# writeRaster(LSR_tiff_UTM, filename = "data/GIS/LSR_tiff_UTM.tif")

#extract elevations of cross-sections from raster
# xsxn_elevation <- extract(LSR_tiff, LSR_xsxns_object)

# save(xsxn_elevation, file = "data/GIS/xsxn_elevation.rda")


# Review xsxns and save final files ---------------------------------------

load("data/GIS/xsxn_elevation.rda")

as.data.frame(xsxn_elevation)

#Seems like having cross-sections with different lengths may be a problem (differing numbers of rows). Check with Ryan whether this is something we can resolve, or if I need to do this again. 