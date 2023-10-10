# Author: Emily Marie Purvis
# Date: 9.30.2023
# Goal: Write code snippits that modify contributor datasets to the standardized OFO field data schema

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data")

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)

#### Load data ####

# for a csv file

treedata = read.csv("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\1_received-data\\{0001}\\data\\vp_ctlsierra2022_v1_trees.csv", header = TRUE)

# for an excel file

# treedata = read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\1_received-data\\{0001}\\data\\vp_ctlsierra2022_v1_trees.xlsx"),sheet=1,col_names = TRUE)

#### Live/Dead to L/D/M ####

treedata$live_dead[treedata$live_dead == '1'] <- 'L'

treedata$live_dead[treedata$live_dead == '2'] <- 'D'

#### DBH in inches to DBH in centimeters ####

# add a new column for DBH in centimeters

treedata <- treedata %>%
  add_column(DBH_cm = "")

# calculate 

treedata$DBH_cm = treedata$dbh * 2.54

#### height in feet to height in meters ####

# add a new column for height in meters

treedata <- treedata %>%
  add_column(height_m = "")

# calculate 

treedata$height_m = treedata$height * 0.3048

#### horizontal distance in feet to horizontal distance in meters ####

# add a new column for distance in meters

treedata <- treedata %>%
  add_column(distance_m = "")

# calculate 

treedata$distance_m = treedata$distance * 0.3048

#### Calculating tree ohvis from crown position ####

# add a new column for ohvis

treedata <- treedata %>%
  add_column(ohvis = "")

# calculate ohvis from canopy position

treedata$ohvis <- case_match(
  treedata$canopy_position,
  c(1, 2, 3) ~ "TRUE",
  c(4, 5) ~ "FALSE",
  .default = ""
)

#### Horizontal distance from plot center to tree surface to horizontal distance from plot center to tree pith ####

# add a new column for horizontal distance to pith

treedata <- treedata %>%
  add_column(horiz_distance_pith = "")

# calculate

treedata$horiz_distance_pith = treedata$distance_m + (0.5 * 0.01 * treedata$DBH_cm)

#### Find NAs to examine them; delete NAs ####

# Always check out the NAs before you delete them!! They could have an important meaning

# for character NAs
treedata$`SNAG DEC`[treedata$`SNAG DEC` == 'NA'] <- ''

# for real NAs
treedata$`SNAG DEC`[is.na(treedata$`SNAG DEC`)] <- ""

#### Export to csv ####

write.csv(treedata, "C:\\Users\\emily\\Desktop\\ALTERED_vp_ctlsierra2022_v1_trees.csv")

# this is a working document! make sure you save it somewhere out of the way

