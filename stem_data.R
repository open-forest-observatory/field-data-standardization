# Author: Emily Marie Purvis
# Date: 8.13.2023
# Goal: convert stem_data.xls individual tree coordinates from distance/azimuth to easting/northing

# What we're working with: stem_data.xls has four sheets with different sites: Site 14, Site 15, Site 11, and Site 10. Each sheet has multiple subplots. Each subplot has it's own plot center with unique easting/northing. Each individual tree within each subplot has a distance (in meters) and azimuth (in degrees) from plot center.

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(pracma)
library(dplyr)

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field data standardization")

#### Going to begin with sheet 1 ####

#### Load and inspect data ####

# Import sheet 1
site14 = read_excel(data("stem_data.xls"),sheet=1,col_names = TRUE,skip=2)

#create a dataframe of subplot centers-- doing this manually is not ideal but I'm unsure of how to extract random cells to make them into a new table
subplot_centers <- matrix(c('NW4', -122.447291, 43.597599, 'NE3', -122.446917, 43.597579, 'SW2', -122.444723, 43.597352, 'SE1', -122.446992, 43.597423, 'NW2', -122.447283, 43.59783, 'NE1', -122.44688, 43.597892), ncol=3, byrow=TRUE)
colnames(subplot_centers) <- c('SUBPLOT','PlotCenterEasting','PlotCenterNorthing')
subplot_centers <- as.data.frame.matrix(subplot_centers)

#### Map plot centers and save them ####
subplot_centers_sp <- st_as_sf(subplot_centers, coords = c("PlotCenterEasting", "PlotCenterNorthing"), crs = 4326, remove=F)

# Transforming spatial data points into EPSG 3857 so it's compatible with default qgis OpenStreetMaps projection
st_write (st_transform (subplot_centers_sp, 3857), "C:\\Users\\emily\\Box\\FOCAL\\field data standardization\\stem_data_site14plotcenters.geojson", delete_dsn=TRUE)

# convert the plot center coordinates to UTM10N format so the calculations in the next step work

subplot_centers_sp_UTM10N <- st_transform (subplot_centers_sp, 32610)

UTM10N_plotcoords <- data.frame(subplot_centers_sp$SUBPLOT, st_coordinates(subplot_centers_sp_UTM10N[,1], st_coordinates(subplot_centers_sp_UTM10N[,2]))) %>% rename (SUBPLOT=subplot_centers_sp.SUBPLOT, EastingUTM10N=X, NorthingUTM10N=Y)

subplot_centers_sp_UTM10N <- full_join(subplot_centers_sp_UTM10N, UTM10N_plotcoords, by="SUBPLOT")

#### give trees ID numbers ####

site14_trees = site14_trees %>% mutate(tree_id = 1:nrow(site14_trees))

#### Get the coordinates of each tree in Site 14 ####

# add new columns to the Site 14 dataframe so that each individual tree has a populated plot center easting and northing

trees_locations = full_join(site14,st_drop_geometry(subplot_centers_sp_UTM10N),by="SUBPLOT")

# calculate the coordinates of each tree using the plot center, distance, and azimuth 

site14_trees = trees_locations %>%
  mutate(TreeEasting = as.numeric(EastingUTM10N) + sin(deg2rad(AZIMUTH)) * `DIST (m)`,
         TreeNorthing = as.numeric(NorthingUTM10N) + cos(deg2rad(AZIMUTH)) * `DIST (m)`)

#### convert to spatial data and export ####

trees_sp <- st_as_sf(site14_trees, coords = c("TreeEasting","TreeNorthing"), crs = 32610)

st_write(trees_sp %>% st_transform(3857),data("C:\\Users\\emily\\Box\\FOCAL\\field data standardization\\stem_data_site14trees.geojson"),delete_dsn=TRUE)

#### sheet 2 / site 15 ####

#### Load and inspect data ####

# Import sheet 1
site15 = read_excel(data("stem_data.xls"),sheet=2,col_names = TRUE,skip=2)

# Remove trees with NA distance and/or azimuth
site15 <- site15 %>% drop_na (AZIMUTH) %>% drop_na (`DIST (m)`)

#create a dataframe of subplot centers-- doing this manually is not ideal but I'm unsure of how to extract random cells to make them into a new table
subplot_centers_site15 <- matrix(c('S2', -122, -24.0664, 43, 35.8758, 'E1', -122, -24.0618, 43, 35.8983, 'W4', -122, -24.0853, 43, 35.8865, 'N3', -122, -24.0726, 43, 35.8902, '6', -122, -24.0992, 43, 35.8932, '5', -122, -24.0931, 43, 35.8992), ncol=5, byrow=TRUE)

colnames(subplot_centers_site15) <- c('SUBPLOT','PlotCenterEastingDegrees', 'PlotCenterEastingMinutes', 'PlotCenterNorthingDegrees', 'PlotCenterNorthingMinutes')

subplot_centers_site15 <- as.data.frame.matrix(subplot_centers_site15)

subplot_centers_site15$PlotCenterEasting = (as.numeric(subplot_centers_site15$PlotCenterEastingDegrees)) + ((as.numeric(subplot_centers_site15$PlotCenterEastingMinutes))/60)

subplot_centers_site15$PlotCenterNorthing = (as.numeric(subplot_centers_site15$PlotCenterNorthingDegrees)) + ((as.numeric(subplot_centers_site15$PlotCenterNorthingMinutes))/60)

#### Map plot centers and save them ####
subplot_centers_site15_sp <- st_as_sf(subplot_centers_site15, coords = c("PlotCenterEasting", "PlotCenterNorthing"), crs = 4326, remove=F)

# Transforming spatial data points into EPSG 3857 so it's compatible with default qgis OpenStreetMaps projection
st_write (st_transform (subplot_centers_site15_sp, 3857), "C:\\Users\\emily\\Box\\FOCAL\\field data standardization\\stem_data_site15plotcenters.geojson", delete_dsn=TRUE)

# convert the plot center coordinates to UTM10N format so the calculations in the next step work

subplot_centers_site15_sp_UTM10N <- st_transform (subplot_centers_site15_sp, 32610)

UTM10N_plotcoords_site15 <- data.frame(subplot_centers_site15_sp$SUBPLOT, st_coordinates(subplot_centers_site15_sp_UTM10N[,1], st_coordinates(subplot_centers_site15_sp_UTM10N[,2]))) %>% rename (SUBPLOT=subplot_centers_site15_sp.SUBPLOT, EastingUTM10N=X, NorthingUTM10N=Y)

subplot_centers_site15_sp_UTM10N <- full_join(subplot_centers_site15_sp_UTM10N, UTM10N_plotcoords_site15, by="SUBPLOT")

#### Get the coordinates of each tree in Site 15 ####

# add new columns to the Site 15 dataframe so that each individual tree has a populated plot center easting and northing

trees_locations_site15 = full_join(site15,st_drop_geometry(subplot_centers_site15_sp_UTM10N),by="SUBPLOT")

# calculate the coordinates of each tree using the plot center, distance, and azimuth

site15_trees = trees_locations_site15 %>%
  mutate(TreeEasting = as.numeric(EastingUTM10N) + sin(deg2rad(AZIMUTH)) * `DIST (m)`,
         TreeNorthing = as.numeric(NorthingUTM10N) + cos(deg2rad(AZIMUTH)) * `DIST (m)`)

#### Give trees ID numbers ####

site15_trees = site15_trees %>% mutate(tree_id = site15_trees$SERIES)

#### convert to spatial data and export ####

trees_site15_sp <- st_as_sf(site15_trees, coords = c("TreeEasting","TreeNorthing"), crs = 32610)

st_write(trees_site15_sp %>% st_transform(3857),data("C:\\Users\\emily\\Box\\FOCAL\\field data standardization\\stem_data_site15trees.geojson"),delete_dsn=TRUE)

#### FOR ALL SITES: make another file that drops duplicate trees ####

