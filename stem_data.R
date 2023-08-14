# Author: Emily Marie Purvis
# Date: 8.13.2023
# Goal: convert stem_data.xls individual tree coordinates from distance/azimuth to easting/northing

# What we're working with: stem_data.xls has four sheets with different sites: Site 14, Site 15, Site 11, and Site 10. Each sheet has multiple subplots. Each subplot has it's own plot center with unique easting/northing. Each individual tree within each subplot has a distance (in meters) and azimuth (in degrees) from plot center.

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(pracma)

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field data standardization")

# Going to begin with sheet 1. Site 14 has six subplots: NW4, NE3, SW2, SE1, NW2, NE1

#### Load and inspect data ####

# Import sheet 1
site14 = read_excel(data("stem_data.xls"),sheet=1,col_names = TRUE,skip=2)

# These sites are in UTM zone 10N (crs = 32610) in Willamette NF, the northings are correct but the eastings should be -122...
# Some of the subplots are in dd.ddddd° format, others are in dd° mm.mmm' format. I converted them all into decimal format manually-- not ideal, I should do it via R function in the future

#create a dataframe of subplot centers-- again, doing this manually is not ideal but I'm unsure of how to extract random cells to make them into a new table
subplot_centers <- matrix(c('NW4', -122.447291, 43.597599, 'NE3', -122.446917, 43.597579, 'SW2', -122.444723, 43.597352, 'SE1', -122.446992, 43.597423, 'NW2', -122.447283, 43.59783, 'NE1', -122.44688, 43.597892), ncol=3, byrow=TRUE)
colnames(subplot_centers) <- c('SUBPLOT','PlotCenterEasting','PlotCenterNorthing')
subplot_centers <- as.data.frame.matrix(subplot_centers)

#### Map plot centers and save them ####
subplot_centers_sp <- st_as_sf(subplot_centers, coords = c("PlotCenterEasting", "PlotCenterNorthing"), crs = 32610, remove=F)
plot(st_geometry(subplot_centers_sp), axes=TRUE)

# Transforming spatial data points into EPSG 3857 so it's compatible with standard qgis OpenStreetMaps projection
st_write (st_transform (subplot_centers_sp, 3857), "C:\\Users\\emily\\Box\\FOCAL\\field data standardization\\stem_data_site14plotcenters.geojson", delete_dsn=TRUE)

# I'm messing something up in this section-- when I bring the geojson into qgis the points are all in some new null island off the coast of s america. Tried various things with no success: also get null island with no crs transformation, tried a lot of coding grammar changes, tried using st_write and st_geometry functions in various ways, tried shapefiles...

#### Get the coordinates of each tree in Site 14 ####

# add new columns to the Site 14 sheet so that each individual tree has a populated easting and northing

trees_locations = full_join(site14,subplot_centers,by="SUBPLOT")

# calculate the coordinates of each tree using the plot center, distance, and azimuth 

site14_trees = trees_locations %>%
  mutate(TreeEasting = as.numeric(PlotCenterEasting) + sin(deg2rad(AZIMUTH)) * `DIST (m)`,
         TreeNorthing = as.numeric(PlotCenterNorthing) + cos(deg2rad(AZIMUTH)) * `DIST (m)`)

# that did not work, the tree eastings and northings are clearly very wrong, trying something else

# Calculate theta from azimuth
trees_locations$theta = trees_locations$AZIMUTH * -1 + 90

# Use theta and plot center coordinates to calculate tree coordinates
trees_locations$TreeEasting = as.numeric(trees_locations$PlotCenterEasting) + trees_locations$`DIST (m)` * cos(trees_locations$theta*pi/180)

trees_locations$TreeNorthing = as.numeric(trees_locations$PlotCenterNorthing) + trees_locations$`DIST (m)` * sin(trees_locations$theta*pi/180)

# okay that gave me the same wrong answers....maybe it's something with using decimal degrees instead of degrees/minutes/seconds



