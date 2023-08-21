# Author: Emily Marie Purvis
# Date: 8.13.2023
# Goal: convert StemData_Batch1.xls (TNC) individual tree coordinates from distance/azimuth to easting/northing

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\Stem_Batch_1")

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(dplyr)

#### Load and inspect data ####

treedata = read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\Stem_Batch_1\\Plot_Data\\StemData_Batch1.xlsx"),sheet=1,col_names = TRUE)

largeplotdata = st_read("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\Stem_Batch_1\\large-plots_for-iri.kml")

smallplotdata = st_read("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\Stem_Batch_1\\small-plots_for-iri_v2.kml")

# need to reformat some of the plot numbers in treedata because they're in different formats

treedata$`Plot#`[treedata$`Plot#` == 'L-525'] <- 'L525'
treedata$`Plot#`[treedata$`Plot#` == 'L-515'] <- 'L515'
treedata$`Plot#`[treedata$`Plot#` == 'L-511'] <- 'L511'
treedata$`Plot#`[treedata$`Plot#` == 'L-512'] <- 'L512'

# need to delete random rows of all NAs in treedata

treedata <- treedata %>% drop_na(`Plot#`)

# R and I both get easily confused about the % slope column so I'm renaming it

treedata <- treedata %>% rename("PercentSlope" = `% slope`)

# some of the trees were measured with horizontal distance, some with slope + slope distance, some with both. we want all trees to have horizontal distance.


# a function that does something like: if horizontal distance is NA, then (calculation with slope and slope distance), then put that value in the horizontal distance column

for(i in 1:nrow(treedata)) {
  if(is.na(treedata$`H Distance`[i])) {
    treedata$HorizontalDistance = (treedata$`Slope distance`)*cos(atan(treedata$PercentSlope/100))
  }
  else {
    treedata$HorizontalDistance = treedata$`H Distance`
  }
}

















# Merge treedata, smallplotdata, and largeplotdata into 1 spatial dataframe

# STILL NEED TO MERGE WITH TREEDATA ONCE YOU FIGURE OUT HOW TO GET THAT DUMB FOR LOOP WORKING

allplots <- rbind(largeplotdata, smallplotdata)


# st_crs (largeplotdata)
# st_crs (smallplotdata)
# EPSG of the plot data is 4326

# convert the plot center coordinates to UTM10N format so the calculations in the next step work

allplots_UTM10N <- st_transform (allplots, 32610)

#### Calculate the coordinates of each tree ####

# add new columns to the treedata dataframe so that each individual tree has a populated plot center easting and northing
