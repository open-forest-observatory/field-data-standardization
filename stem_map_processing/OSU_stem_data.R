# Author: Emily Marie Purvis
# Date: 8.13.2023
# Goal: convert stem_data.xls (Johnston, Oregon State University) individual tree coordinates from distance/azimuth to easting/northing

# What we're working with: stem_data.xls has four sheets with different sites: Site 14, Site 15, Site 11, and Site 10. Each sheet has multiple subplots. Each subplot has it's own plot center with unique easting/northing. Each individual tree within each subplot has a distance (in meters) and azimuth (in degrees) from plot center.

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(pracma)
library(dplyr)

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\OSU.stem_data")

#### Going to begin with sheet 1 ####

#### Load and inspect data ####

# Import sheet 1
site14 = read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\OSU.stem_data\\stem_data.xls"),sheet=1,col_names = TRUE,skip=2)

#create a dataframe of subplot centers-- doing this manually is not ideal but I'm unsure of how to extract random cells to make them into a new table
subplot_centers <- matrix(c('NW4', -122.447291, 43.597599, 'NE3', -122.446917, 43.597579, 'SW2', -122.444723, 43.597352, 'SE1', -122.446992, 43.597423, 'NW2', -122.447283, 43.59783, 'NE1', -122.44688, 43.597892), ncol=3, byrow=TRUE)
colnames(subplot_centers) <- c('SUBPLOT','PlotCenterEasting','PlotCenterNorthing')
subplot_centers <- as.data.frame.matrix(subplot_centers)

#### Map plot centers and save them ####
subplot_centers_sp <- st_as_sf(subplot_centers, coords = c("PlotCenterEasting", "PlotCenterNorthing"), crs = 4326, remove=F)

# Transforming spatial data points into EPSG 3857 so it's compatible with default qgis OpenStreetMaps projection
st_write (st_transform (subplot_centers_sp, 4326), "C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\OSU.stem_data\\stem_data_site14plotcenters.gpkg", delete_dsn=TRUE)

# convert the plot center coordinates to UTM10N format so the calculations in the next step work

subplot_centers_sp_UTM10N <- st_transform (subplot_centers_sp, 32610)

UTM10N_plotcoords <- data.frame(subplot_centers_sp$SUBPLOT, st_coordinates(subplot_centers_sp_UTM10N[,1], st_coordinates(subplot_centers_sp_UTM10N[,2]))) %>% rename (SUBPLOT=subplot_centers_sp.SUBPLOT, EastingUTM10N=X, NorthingUTM10N=Y)

subplot_centers_sp_UTM10N <- full_join(subplot_centers_sp_UTM10N, UTM10N_plotcoords, by="SUBPLOT")

#### Get the coordinates of each tree in Site 14 ####

# add new columns to the Site 14 dataframe so that each individual tree has a populated plot center easting and northing

trees_locations = full_join(site14,st_drop_geometry(subplot_centers_sp_UTM10N),by="SUBPLOT")

# calculate the coordinates of each tree using the plot center, distance, and azimuth 

site14_trees = trees_locations %>%
  mutate(TreeEasting = as.numeric(EastingUTM10N) + sin(deg2rad(AZIMUTH)) * `DIST (m)`,
         TreeNorthing = as.numeric(NorthingUTM10N) + cos(deg2rad(AZIMUTH)) * `DIST (m)`)

#### give trees ID numbers ####

site14_trees = site14_trees %>% mutate(tree_id = 1:nrow(site14_trees))

#### convert to spatial data and export ####

trees_sp <- st_as_sf(site14_trees, coords = c("TreeEasting","TreeNorthing"), crs = 32610)

st_write(trees_sp %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\OSU.stem_data\\stem_data_site14trees.gpkg"),delete_dsn=TRUE)

#### sheet 2 / site 15 ####

#### Load and inspect data ####

# Import sheet 2
site15 = read_excel(data("stem_data.xls"),sheet=2,col_names = TRUE,skip=2)

# Remove trees with NA distance and/or azimuth
site15 <- site15 %>% drop_na (AZIMUTH) %>% drop_na (`DIST (m)`)

#create a dataframe of subplot centers
subplot_centers_site15 <- matrix(c('S2', -122, -24.0664, 43, 35.8758, 'E1', -122, -24.0618, 43, 35.8983, 'W4', -122, -24.0853, 43, 35.8865, 'N3', -122, -24.0726, 43, 35.8902, '6', -122, -24.0992, 43, 35.8932, '5', -122, -24.0931, 43, 35.8992), ncol=5, byrow=TRUE)

colnames(subplot_centers_site15) <- c('SUBPLOT','PlotCenterEastingDegrees', 'PlotCenterEastingMinutes', 'PlotCenterNorthingDegrees', 'PlotCenterNorthingMinutes')

subplot_centers_site15 <- as.data.frame.matrix(subplot_centers_site15)

subplot_centers_site15$PlotCenterEasting = (as.numeric(subplot_centers_site15$PlotCenterEastingDegrees)) + ((as.numeric(subplot_centers_site15$PlotCenterEastingMinutes))/60)

subplot_centers_site15$PlotCenterNorthing = (as.numeric(subplot_centers_site15$PlotCenterNorthingDegrees)) + ((as.numeric(subplot_centers_site15$PlotCenterNorthingMinutes))/60)

#### Map plot centers and save them ####
subplot_centers_site15_sp <- st_as_sf(subplot_centers_site15, coords = c("PlotCenterEasting", "PlotCenterNorthing"), crs = 4326, remove=F)

# Transforming spatial data points into EPSG 3857 so it's compatible with default qgis OpenStreetMaps projection
st_write (st_transform (subplot_centers_site15_sp, 4326), "C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\OSU.stem_data\\stem_data_site15plotcenters.gpkg", delete_dsn=TRUE)

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

st_write(trees_site15_sp %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\OSU.stem_data\\stem_data_site15trees.gpkg"),delete_dsn=TRUE)

#### sheet 3 / site 11 ####

#### Load and inspect data ####

# Import sheet 3
site11 = read_excel(data("stem_data.xls"),sheet=3,col_names = TRUE,skip=2)

# Remove trees with NA distance and/or azimuth
site11 <- site11 %>% drop_na (AZIMUTH) %>% drop_na (`DIST (m)`)

#create a dataframe of subplot centers
subplot_centers_site11 <- matrix(c('SW2', -122, -23.5526, 43, 33.8795, 'SE1', -122, -23.5373, 43, 33.8782, 'NW4', -122, -23.5600, 43, 33.8777, 'NE3', -122, -23.5377, 43, 33.8937, '6', -122, -23.5545, 43, 33.9010, '5', -122, -23.5342, 43, 33.8985), ncol=5, byrow=TRUE)

colnames(subplot_centers_site11) <- c('SUBPLOT','PlotCenterEastingDegrees', 'PlotCenterEastingMinutes', 'PlotCenterNorthingDegrees', 'PlotCenterNorthingMinutes')

subplot_centers_site11 <- as.data.frame.matrix(subplot_centers_site11)

subplot_centers_site11$PlotCenterEasting = (as.numeric(subplot_centers_site11$PlotCenterEastingDegrees)) + ((as.numeric(subplot_centers_site11$PlotCenterEastingMinutes))/60)

subplot_centers_site11$PlotCenterNorthing = (as.numeric(subplot_centers_site11$PlotCenterNorthingDegrees)) + ((as.numeric(subplot_centers_site11$PlotCenterNorthingMinutes))/60)

#### Map plot centers and save them ####
subplot_centers_site11_sp <- st_as_sf(subplot_centers_site11, coords = c("PlotCenterEasting", "PlotCenterNorthing"), crs = 4326, remove=F)

st_write (st_transform (subplot_centers_site11_sp, 4326), "C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\OSU.stem_data\\stem_data_site11plotcenters.gpkg", delete_dsn=TRUE)

# convert the plot center coordinates to UTM10N format so the calculations in the next step work

subplot_centers_site11_sp_UTM10N <- st_transform (subplot_centers_site11_sp, 32610)

UTM10N_plotcoords_site11 <- data.frame(subplot_centers_site11_sp$SUBPLOT, st_coordinates(subplot_centers_site11_sp_UTM10N[,1], st_coordinates(subplot_centers_site11_sp_UTM10N[,2]))) %>% rename (SUBPLOT=subplot_centers_site11_sp.SUBPLOT, EastingUTM10N=X, NorthingUTM10N=Y)

subplot_centers_site11_sp_UTM10N <- full_join(subplot_centers_site11_sp_UTM10N, UTM10N_plotcoords_site11, by="SUBPLOT")

#### Get the coordinates of each tree in Site 11 ####

# add new columns to the Site 11 dataframe so that each individual tree has a populated plot center easting and northing

trees_locations_site11 = full_join(site11,st_drop_geometry(subplot_centers_site11_sp_UTM10N),by="SUBPLOT")

# calculate the coordinates of each tree using the plot center, distance, and azimuth

site11_trees = trees_locations_site11 %>%
  mutate(TreeEasting = as.numeric(EastingUTM10N) + sin(deg2rad(AZIMUTH)) * `DIST (m)`,
         TreeNorthing = as.numeric(NorthingUTM10N) + cos(deg2rad(AZIMUTH)) * `DIST (m)`)

#### Give trees ID numbers ####

site11_trees = site11_trees %>% mutate(tree_id = site11_trees$SERIES)

#### convert to spatial data and export ####

trees_site11_sp <- st_as_sf(site11_trees, coords = c("TreeEasting","TreeNorthing"), crs = 32610)

st_write(trees_site11_sp %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\OSU.stem_data\\stem_data_site11trees.gpkg"),delete_dsn=TRUE)

#### sheet 4 / site 10 ####

#### Load and inspect data ####

# Import sheet 4
site10 = read_excel(data("stem_data.xls"),sheet=4,col_names = TRUE,skip=2)

# Remove trees with NA distance and/or azimuth
site10 <- site10 %>% drop_na (AZIMUTH) %>% drop_na (`DIST (m)`)

#create a dataframe of subplot centers-- doing this manually is not ideal but I'm unsure of how to extract random cells to make them into a new table
subplot_centers_site10 <- matrix(c('NW1', -122, -26.9530, 43, 33.6189, 'NE2', -122, -26.9322, 43, 33.6183, 'SE4', -122, -26.9307, 43, 33.6112, 'SW3', -122, -26.9527, 43, 33.6072, '5', -122, -26.9485, 43, 33.5955, '6', -122, -26.9319, 43, 33.5941), ncol=5, byrow=TRUE)

colnames(subplot_centers_site10) <- c('SUBPLOT','PlotCenterEastingDegrees', 'PlotCenterEastingMinutes', 'PlotCenterNorthingDegrees', 'PlotCenterNorthingMinutes')

subplot_centers_site10 <- as.data.frame.matrix(subplot_centers_site10)

subplot_centers_site10$PlotCenterEasting = (as.numeric(subplot_centers_site10$PlotCenterEastingDegrees)) + ((as.numeric(subplot_centers_site10$PlotCenterEastingMinutes))/60)

subplot_centers_site10$PlotCenterNorthing = (as.numeric(subplot_centers_site10$PlotCenterNorthingDegrees)) + ((as.numeric(subplot_centers_site10$PlotCenterNorthingMinutes))/60)

#### Map plot centers and save them ####
subplot_centers_site10_sp <- st_as_sf(subplot_centers_site10, coords = c("PlotCenterEasting", "PlotCenterNorthing"), crs = 4326, remove=F)

# Transforming spatial data points into EPSG 3857 so it's compatible with default qgis OpenStreetMaps projection
st_write (st_transform (subplot_centers_site10_sp, 4326), "C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\OSU.stem_data\\stem_data_site10plotcenters.gpkg", delete_dsn=TRUE)

# convert the plot center coordinates to UTM10N format so the calculations in the next step work

subplot_centers_site10_sp_UTM10N <- st_transform (subplot_centers_site10_sp, 32610)

UTM10N_plotcoords_site10 <- data.frame(subplot_centers_site10_sp$SUBPLOT, st_coordinates(subplot_centers_site10_sp_UTM10N[,1], st_coordinates(subplot_centers_site10_sp_UTM10N[,2]))) %>% rename (SUBPLOT=subplot_centers_site10_sp.SUBPLOT, EastingUTM10N=X, NorthingUTM10N=Y)

subplot_centers_site10_sp_UTM10N <- full_join(subplot_centers_site10_sp_UTM10N, UTM10N_plotcoords_site10, by="SUBPLOT")

#### Get the coordinates of each tree in Site 10 ####

# add new columns to the Site 10 dataframe so that each individual tree has a populated plot center easting and northing

trees_locations_site10 = full_join(site10,st_drop_geometry(subplot_centers_site10_sp_UTM10N),by="SUBPLOT")

# calculate the coordinates of each tree using the plot center, distance, and azimuth

site10_trees = trees_locations_site10 %>%
  mutate(TreeEasting = as.numeric(EastingUTM10N) + sin(deg2rad(AZIMUTH)) * `DIST (m)`,
         TreeNorthing = as.numeric(NorthingUTM10N) + cos(deg2rad(AZIMUTH)) * `DIST (m)`)

#### Give trees ID numbers ####

site10_trees = site10_trees %>% mutate(tree_id = site10_trees$SERIES)

#### convert to spatial data and export ####

trees_site10_sp <- st_as_sf(site10_trees, coords = c("TreeEasting","TreeNorthing"), crs = 32610)

st_write(trees_site10_sp %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\OSU.stem_data\\stem_data_site10trees.gpkg"),delete_dsn=TRUE)

#### FOR ALL SITES: make another file that drops duplicate trees ####

# site 10

site10_trees$duplicated = duplicated(site10_trees %>%  select( SPECIES, TreeEasting, TreeNorthing))

# no duplicates....

# site 11

site11_trees$duplicated = duplicated(site11_trees %>%  select( SPECIES, TreeEasting, TreeNorthing))

# no duplicates

# site 15

site15_trees$duplicated = duplicated(site15_trees %>%  select(SPECIES, TreeEasting, TreeNorthing))

# the one found pair of duplicates appears to be different trees-- they are in the same subplot, same spp and condition but different DBHs. I don't think the crew would duplicate within the same plot, the different DBHs is enough to confirm this is not a true duplicate

# site 14

site14_trees$duplicated = duplicated(site14_trees %>%  select(SPECIES, TreeEasting, TreeNorthing))

# the one found pair of duplicates appears to be different trees-- they are in the same subplot, same spp but different DBHs and different conditions. I don't think the crew would duplicate within the same plot, the different DBHs/conditions is enough to confirm this is not a true duplicate

#### create more condensed spatial output files ####

#### all trees ####

# rename bark fissure depth columns-- there are four and only the first one retained column headings; make sure all matching columns have the same titles (not the same in the original data sheet) and same format

trees_sp <- trees_sp %>% rename("BARK FISSURE DEPTH 2 (mm)" = "...13", "BARK FISSURE DEPTH 3 (mm)" = "...14", "BARK FISSURE DEPTH 4 (mm)" = "...15")
trees_sp$PlotCenterEasting = as.numeric(as.character(trees_sp$PlotCenterEasting))
trees_sp$PlotCenterNorthing = as.numeric(as.character(trees_sp$PlotCenterNorthing))

trees_site15_sp <- trees_site15_sp %>% rename("BARK FISSURE DEPTH 2 (mm)" = "...13", "BARK FISSURE DEPTH 3 (mm)" = "...14", "BARK FISSURE DEPTH 4 (mm)" = "...15")
trees_site15_sp <- trees_site15_sp %>% rename ("DBH (cm)" = "DBH", "TREE HEIGHT (m)" = "TREE HEIGHT", "HEIGHT TO LIVE FOLIAGE (m)" = "HEIGHT TO LIVE FOLIAGE", "HEIGHT TO DEAD BRANCH (m)" = "HEIGHT TO DEAD BRANCH", "BARK FISSURE DEPTH (mm)" = "BARK FISSURE DEPTH", "CROWN TO BASE HEIGHT (m)" = "CROWN TO BASE HEIGHT")

trees_site11_sp <- trees_site11_sp %>% rename("BARK FISSURE DEPTH 2 (mm)" = "...13", "BARK FISSURE DEPTH 3 (mm)" = "...14", "BARK FISSURE DEPTH 4 (mm)" = "...15")
trees_site11_sp <- trees_site11_sp %>% rename ("DBH (cm)" = "DBH", "TREE HEIGHT (m)" = "TREE HEIGHT", "HEIGHT TO LIVE FOLIAGE (m)" = "HEIGHT TO LIVE FOLIAGE", "HEIGHT TO DEAD BRANCH (m)" = "HEIGHT TO DEAD BRANCH", "BARK FISSURE DEPTH (mm)" = "BARK FISSURE DEPTH", "CROWN TO BASE HEIGHT (m)" = "CROWN TO BASE HEIGHT")

trees_site10_sp <- trees_site10_sp %>% rename("BARK FISSURE DEPTH 2 (mm)" = "...13", "BARK FISSURE DEPTH 3 (mm)" = "...14", "BARK FISSURE DEPTH 4 (mm)" = "...15")
trees_site10_sp <- trees_site10_sp %>% rename ("DBH (cm)" = "DBH", "TREE HEIGHT (m)" = "TREE HEIGHT", "HEIGHT TO LIVE FOLIAGE (m)" = "HEIGHT TO LIVE FOLIAGE", "HEIGHT TO DEAD BRANCH (m)" = "HEIGHT TO DEAD BRANCH", "BARK FISSURE DEPTH (mm)" = "BARK FISSURE DEPTH", "CROWN TO BASE HEIGHT (m)" = "CROWN TO BASE HEIGHT")

# make sure all the trees data sets have the same columns-- one above I did in decimal degrees and one I did in degrees decimal minutes...a lesson in consistency 

treeskeep <- c("SUBPLOT", "SPECIES", "CONDITION        (LI, ST, SN, LO)", "DECAY CLASS (ST, SN only)", "DBH (cm)", "TREE HEIGHT (m)", "HEIGHT TO LIVE FOLIAGE (m)", "HEIGHT TO DEAD BRANCH (m)", "BARK FISSURE DEPTH (mm)", "BARK FISSURE DEPTH 2 (mm)", "BARK FISSURE DEPTH 3 (mm)", "BARK FISSURE DEPTH 4 (mm)", "DIST (m)", "AZIMUTH", "CROWN TO BASE HEIGHT (m)", "NOTES", "PlotCenterEasting", "PlotCenterNorthing","EastingUTM10N", "NorthingUTM10N", "tree_id", "geometry" )

trees_sp <- trees_sp[treeskeep]
trees_site15_sp <- trees_site15_sp[treeskeep]
trees_site11_sp <- trees_site11_sp[treeskeep]
trees_site10_sp <- trees_site10_sp[treeskeep]

# create a spatial dataframe of all the trees and export it in ESPG 3857

alltrees <- bind_rows (trees_site10_sp, trees_site11_sp, trees_site15_sp, trees_sp)

st_write(alltrees %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\OSU.stem_data\\stem_data_alltrees.gpkg"),delete_dsn=TRUE)

#### all plots ####

# add a column with the site number to all the spatial dataframes

subplot_centers_sp$SITE = 14
subplot_centers_site15_sp$SITE = 15
subplot_centers_site11_sp$SITE = 11
subplot_centers_site10_sp$SITE = 10

# make sure all the columns have the same format between dataframes

subplot_centers_sp$PlotCenterEasting = as.numeric(as.character(subplot_centers_sp$PlotCenterEasting))
subplot_centers_sp$PlotCenterNorthing = as.numeric(as.character(subplot_centers_sp$PlotCenterNorthing))

# make sure all the dataframes have the same columns

plotskeep <- c("SUBPLOT", "PlotCenterEasting", "PlotCenterNorthing", "geometry", "SITE")

subplot_centers_sp <- subplot_centers_sp[plotskeep]
subplot_centers_site15_sp <- subplot_centers_site15_sp[plotskeep]
subplot_centers_site11_sp <- subplot_centers_site11_sp[plotskeep]
subplot_centers_site10_sp <- subplot_centers_site10_sp[plotskeep]

# create a spatial dataframe of all the subplot centers and export it in ESPG 3857

allplots <- bind_rows (subplot_centers_sp, subplot_centers_site15_sp, subplot_centers_site11_sp, subplot_centers_site10_sp)

st_write(allplots %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\OSU.stem_data\\stem_data_allplots.gpkg"),delete_dsn=TRUE)
