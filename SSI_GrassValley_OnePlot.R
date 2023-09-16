# Author: Emily Marie Purvis
# Date: 8.25.2023
# Goal: convert SSI's Grass Valley Plot 1 individual tree coordinates from distance/azimuth to easting/northing

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\SSI.GrassValley")

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(pracma)

#### Load and inspect data; small tweaks to get it standardized and ready for calculating tree coordinates ####

treedata = read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\SSI.GrassValley\\FOCAL_LAB_OnePlot_SSI_Corrected.xlsx"),sheet=3,col_names = TRUE)

treedata$Distance_meters = treedata$DISTANCE_FT*0.3048

# some azimuth measurements are in one column and some are in another, making a new column that has all the correct azimuth measurements

treedata$Azimuth4Real <- 0

for(i in 1:nrow(treedata)) {
  if(is.na(treedata$AZM_corrected[i])) {
    treedata[i,]$Azimuth4Real = (treedata[i,]$AZM)
  }
  else {
    treedata[i,]$Azimuth4Real = treedata[i,]$AZM_corrected
  }
}

#### plot coordinates from Derek #### 

# plotdata = st_read("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\SSI.GrassValley\\site-points_SS_v1.kml")

# st_crs(plotdata) : Derek's plot coordinates are in EPSG 4326

# convert plot center coordinates to UTM10N so calculations in the next step work

# plots_UTM10N <- st_transform (plotdata, 32610)

# extract the geometry features into columns of lat and long, then merge back into the spatial data frame

# UTM10N_plotcoords <- data.frame(plots_UTM10N$'Name', st_coordinates(plots_UTM10N[,1], st_coordinates(plots_UTM10N[,2]))) %>% rename ('Name'=plots_UTM10N.Name, PlotEastingUTM10N=X, PlotNorthingUTM10N=Y)

# plots_UTM10N <- full_join(plots_UTM10N, UTM10N_plotcoords, by="Name")

#### plot coordinates from Erin at SSI: for the large plot are 39.199336N, 120.905293W ####

# make the plot center coordinates into a spatial dataframe

plotcenter <- matrix(c('1', -120.905293, 39.199336), ncol=3, byrow=TRUE)

colnames(plotcenter) <- c('PLOT_ID','PlotCenterEasting','PlotCenterNorthing')

plotcenter <- as.data.frame.matrix(plotcenter)

plotcenter_sp <- st_as_sf(plotcenter, coords = c("PlotCenterEasting", "PlotCenterNorthing"), crs = 4326, remove=F)

# save the spatial dataframe

st_write (plotcenter_sp, "C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\SSI.GrassValley\\plotcenter1.gpkg", delete_dsn=TRUE)

# convert plot center coordinates to UTM10N so calculations in the next step work

plotcenter_UTM10N <- st_transform (plotcenter_sp, 32610)

# extract the geometry features into columns of lat and long, then merge back into the spatial data frame

UTM10N_plotcoords <- data.frame(plotcenter_UTM10N$'PLOT_ID', st_coordinates(plotcenter_UTM10N[,1], st_coordinates(plots_UTM10N[,2]))) %>% rename ('PLOT_ID'=plotcenter_UTM10N.PLOT_ID, PlotEastingUTM10N=X, PlotNorthingUTM10N=Y)

plotcenter_UTM10N <- full_join(plotcenter_UTM10N, UTM10N_plotcoords, by="PLOT_ID")

#### Calculate the coordinates of each tree ####

# add new columns to the treedata dataframe so that each individual tree has a populated plot center easting and northing

plotcenter_UTM10N$PLOT_ID = as.numeric(as.character(plotcenter_UTM10N$PLOT_ID))

trees_locations = full_join(treedata,st_drop_geometry(plotcenter_UTM10N),by='PLOT_ID')

# calculate the coordinates of each tree using the plot center, horizontal distance, and azimuth 

trees_calcs = trees_locations %>%
  mutate(TreeEasting = as.numeric(PlotEastingUTM10N) + sin(deg2rad(Azimuth4Real)) * Distance_meters,
         TreeNorthing = as.numeric(PlotNorthingUTM10N) + cos(deg2rad(Azimuth4Real)) * Distance_meters)

#### convert to spatial data and export ####

trees_sp <- st_as_sf(trees_calcs, coords = c("TreeEasting","TreeNorthing"), crs = 32610)

# exporting the tree spatial data in the same format as the plot data came in, 4326

st_write(trees_sp %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\SSI.GrassValley\\plot1trees.gpkg"),delete_dsn=TRUE)

