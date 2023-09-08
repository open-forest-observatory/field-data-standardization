# Author: Emily Marie Purvis
# Date: 9.8.2023
# Goal: convert StemData_Batch2.xls (TNC) individual tree coordinates from distance/azimuth to easting/northing

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2")

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(dplyr)
library(pracma)

#### Load and inspect data; small tweaks to get it standardized and ready for calculating tree coordinates ####

treedata = read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\Plot_Data\\StemData_Batch2.xlsx"),sheet=1,col_names = TRUE)

largeplotdata = st_read("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_1\\large-plots_for-iri.kml")

smallplotdata = st_read("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_1\\small-plots_for-iri_v2.kml")

# need to reformat the plot numbers in treedata so they match the plot data 

treedata$`Plot#`[treedata$`Plot#` == 'L-521'] <- 'L521'
treedata$`Plot#`[treedata$`Plot#` == 'L-505'] <- 'L505'
treedata$`Plot#`[treedata$`Plot#` == 'L-504'] <- 'L504'
treedata$`Plot#`[treedata$`Plot#` == 'L-536'] <- 'L536'
treedata$`Plot#`[treedata$`Plot#` == 'L-507'] <- 'L507'
treedata$`Plot#`[treedata$`Plot#` == 'L-502'] <- 'L502'
treedata$`Plot#`[treedata$`Plot#` == 'S-038'] <- 'S038'
treedata$`Plot#`[treedata$`Plot#` == 'L-529'] <- 'L529'
treedata$`Plot#`[treedata$`Plot#` == 'L-508'] <- 'L508'
treedata$`Plot#`[treedata$`Plot#` == 'S-313'] <- 'S313'

# need to delete random rows of all NAs in treedata

treedata <- treedata %>% drop_na(`Plot#`)

# R and I both get easily confused about the % slope column so I'm renaming it

treedata <- treedata %>% rename("PercentSlope" = `% slope`)

# some of the trees were measured with horizontal distance, some with slope + slope distance, some with both. we want all trees to have horizontal distance. 

# create a blank column for horizontal distance

treedata$HorizontalDistance <- 0

# if horizontal distance is NA, then calculate it from percent slope and slope distance, then put that value in the horizontal distance column. if horizontal distance is not NA, use the value the crew measured

for(i in 1:nrow(treedata)) {
  if(is.na(treedata$`H Distance`[i])) {
    treedata[i,]$HorizontalDistance = (treedata[i,]$`Slope distance`)*cos(atan(treedata[i,]$PercentSlope/100))
  }
  else {
    treedata[i,]$HorizontalDistance = treedata[i,]$`H Distance`
  }
}

# st_crs (largeplotdata)
# st_crs (smallplotdata)
# EPSG of the plot data is 4326

# convert the plot center coordinates to UTM10N format so the calculations in the next step work. first, merge smallplotdata and largeplotdata into 1 spatial dataframe

allplots <- rbind(largeplotdata, smallplotdata)

allplots_UTM10N <- st_transform (allplots, 32610) %>% rename('Plot#'='Name')

# extract the geometry features into columns of lat and long, then merge back into the spatial data frame

UTM10N_plotcoords <- data.frame(allplots_UTM10N$'Plot#', st_coordinates(allplots_UTM10N[,1], st_coordinates(allplots_UTM10N[,2]))) %>% rename ('Plot#'=allplots_UTM10N..Plot.., PlotEastingUTM10N=X, PlotNorthingUTM10N=Y)

allplots_UTM10N <- full_join(allplots_UTM10N, UTM10N_plotcoords, by="Plot#")

#### Calculate the coordinates of each tree ####

# add new columns to the treedata dataframe so that each individual tree has a populated plot center easting and northing

trees_locations = full_join(treedata,st_drop_geometry(allplots_UTM10N),by='Plot#') %>% drop_na('HorizontalDistance')

# calculate the coordinates of each tree using the plot center, horizontal distance, and azimuth 

trees_calcs = trees_locations %>%
  mutate(TreeEasting = as.numeric(PlotEastingUTM10N) + sin(deg2rad(AZM)) * HorizontalDistance,
         TreeNorthing = as.numeric(PlotNorthingUTM10N) + cos(deg2rad(AZM)) * HorizontalDistance)

# give trees ID numbers

trees_calcs = trees_calcs %>% mutate(tree_id = 1:nrow(trees_calcs))

#### convert to spatial data and export ####

trees_sp <- st_as_sf(trees_calcs, coords = c("TreeEasting","TreeNorthing"), crs = 32610)

# exporting the tree spatial data in the same format as the plot data came in, 4326

st_write(trees_sp %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\Stem_Batch_2_treecoordinates.gpkg"),delete_dsn=TRUE)
