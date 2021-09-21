
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
library(purrr)
library(plotly)
library(data.table) #used to extract minimum points from xsxns

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

LSR_xsxns <- mapview(LSR_buffer) %>%
  editMap()

LSR_xsxns_object <- LSR_xsxns$finished

LSR_xsxns_sf <- LSR_xsxns_object %>%
   select(geometry)

#view buffer and cross-sections
mapview(LSR_buffer) +
 mapview(LSR_xsxns_object)

#Save object of cross-sections
save(LSR_xsxns_object, file = "data/GIS/LSR_xsxns.rda")



# SKIP - Load raster tiff and convert projection to UTM -------------------

#Following lines are to load the TIF and convert to UTM, which was already completed on the first run. Commented out, but saved for posterity.

#Tif of Little Shasta lidar is saved in X:\ShastaRiver\SpatialData\ImageFiles\Little_Shasta\little_shasta_lidar_dsm1.tif

#library(tiff)

# LSR_path <- "X:/ShastaRiver/SpatialData/ImageFiles/Little_Shasta/little_shasta_lidar_dsm1.tif"
# 
# LSR_tiff=raster(LSR_path)
# 
# #Add original projection to raster
# LSR_nad83 <- "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
# 
# #check projection of raster
# crs(LSR_tiff)
# 
# #Confirm projection of buffer layer
# st_crs(LSR_buffer)

#convert tiff to UTM projection - this takes a REALLY long time, and shouldn't be re-run, if possible. Commented out this and the extraction lines to avoid accidentally running them.
# LSR_tiff_UTM <- projectRaster(LSR_tiff, crs = "+init=EPSG:32610")
# 
# #confirm new projection
# crs(LSR_tiff_UTM)
# 
# #save raster with UTM projection
# writeRaster(LSR_tiff_UTM, filename = "data/GIS/LSR_tiff_UTM.tif")

# Extract xsxn elevations from raster -------------------------------------------------------------

load("data/GIS/LSR_xsxns.rda")

#Plot to make sure cross-sections exist

ggplot() +
  geom_sf(data = LSR_LiDAR_boundary_clip) +
  geom_sf(data = LSR_buffer) +
  geom_sf(data = LSR_xsxns_object) +
  coord_sf()

LSR_tiff_UTM_path <- "C:/Users/fissekis/Documents/GitHub_projects/Little_Shasta_River/data/GIS/LSR_tiff_UTM.tif"

LSR_tiff_UTM=raster(LSR_tiff_UTM_path)

#extract elevations of cross-sections from raster
xsxn_elevation <- extract(LSR_tiff_UTM, LSR_xsxns_object)

save(xsxn_elevation, file = "data/GIS/xsxn_elevation.rda")


# Review xsxns and save final files ---------------------------------------

load("data/GIS/xsxn_elevation.rda")

xsxn_elevation <- set_names(xsxn_elevation, LSR_xsxns_object$X_leaflet_id)
names(xsxn_elevation)

xsxn_list <- map(xsxn_elevation, ~as.data.frame(.x))
xsxn_list <- xsxn_list %>% 
  mutate(rowid = map(., ~mutate(.x, seq(0,nrow(.x), by = 0.4572))))

map(xsxn_list, ~nrow(.x))

xsxn_df <- bind_rows(xsxn_list, .id = "X_leaflet_id") %>% 
  mutate(elevation_m = .x) %>% 
  filter(!is.na(elevation_m)) %>% 
  mutate(X_leaflet_id = as.integer(X_leaflet_id)) %>% 
  group_by(X_leaflet_id) %>% 
  mutate(rowid = row_number()) %>% 
  mutate(location = row_number()*0.4572) #this takes the rowid and scales it to the 1.5 ft interval of the lidar data; multiplying by the m conversion since the layer projections were converted to UTMs

summary(xsxn_df)

xsxn_df %>% 
  group_by(X_leaflet_id) %>% 
  tally() %>% 
  view()

xsxn_sf <- left_join(LSR_xsxns_object, xsxn_df)

ggplotly(
ggplot(data = xsxn_df) +
  geom_line(aes(x=location, y=elevation_m, group = X_leaflet_id, color = X_leaflet_id))
)

#seems like not every cross-section includes the whole stream channel. May need to redraw cross-sections. Note that I can re-import the UTM.tiff file, and not go through the conversion step. This will save a lot of time!
