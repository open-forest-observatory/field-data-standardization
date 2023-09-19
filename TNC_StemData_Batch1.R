# Author: Emily Marie Purvis
# Date: 8.21.2023, 9.12.2023
# Goal: convert StemData_Batch1.xls (TNC) individual tree coordinates from distance/azimuth to easting/northing

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_1\\updated (9.11.23) data and output files\\Stem_Batch_1")

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(pracma)
library(ggrepel)

#### Load and inspect data; small tweaks to get it standardized and ready for calculating tree coordinates ####

# tree data

treedata = read_excel(data("Plot_Data\\StemData_Batch1_emp.edits.xlsx"),sheet=1,col_names = TRUE)

# plot data

L506 <- read_sf('Post_Processed_GPS\\L-506_g\\L-506.shp') 

L506 = L506 %>% mutate(`Plot#` = 'L506')

# st_crs(L506)
# EPSG 4326

L511 <- read_sf('Post_Processed_GPS\\L-511_g\\L-511.shp') 

L511 = L511 %>% mutate(`Plot#` = 'L511')

# st_crs(L511)

L512 <- read_sf('Post_Processed_GPS\\L-512_g\\L-512.shp') 

L512 = L512 %>% mutate(`Plot#` = 'L512')

# st_crs(L512)

L515 <- read_sf('Post_Processed_GPS\\L-515_g\\L-515.shp')

L515 = L515 %>% mutate(`Plot#` = 'L515')

# st_crs(L515)

L517 <- read_sf('Post_Processed_GPS\\L-517_g\\L-517.shp') 

L517 = L517 %>% mutate(`Plot#` = 'L517')

# st_crs(L517)

L525 <- read_sf('Post_Processed_GPS\\L-525_g\\L-525.shp') 

L525 = L525 %>% mutate(`Plot#` = 'L525')

# st_crs(L525)

S107 <- read_sf('Post_Processed_GPS\\S-107_g\\S-107.shp') 

S107 = S107 %>% mutate(`Plot#` = 'S107')

# st_crs(S107)

S109 <- read_sf('Post_Processed_GPS\\S-109_g\\S-109.shp') 

S109 = S109 %>% mutate(`Plot#` = 'S109')

# st_crs(S109)

S113 <- read_sf('Post_Processed_GPS\\S-113_g\\S-113.shp') 

S113 = S113 %>% mutate(`Plot#` = 'S113')

# st_crs(S113)

S118 <- read_sf('Post_Processed_GPS\\S-118_g\\S-118.shp') 

S118 = S118 %>% mutate(`Plot#` = 'S118')

# st_crs(S118)

# need to reformat some of the plot numbers in treedata because they're in different formats

treedata$`Plot#`[treedata$`Plot#` == 'L-506'] <- 'L506'
treedata$`Plot#`[treedata$`Plot#` == 'L-511'] <- 'L511'
treedata$`Plot#`[treedata$`Plot#` == 'L-512'] <- 'L512'
treedata$`Plot#`[treedata$`Plot#` == 'L-515'] <- 'L515'
treedata$`Plot#`[treedata$`Plot#` == 'L-517'] <- 'L517'
treedata$`Plot#`[treedata$`Plot#` == 'L-525'] <- 'L525'
treedata$`Plot#`[treedata$`Plot#` == 'S-107'] <- 'S107'
treedata$`Plot#`[treedata$`Plot#` == 'S-109'] <- 'S109'
treedata$`Plot#`[treedata$`Plot#` == 'S-113'] <- 'S113'
treedata$`Plot#`[treedata$`Plot#` == 'S-118'] <- 'S118'

# need to delete random rows of all NAs in treedata

treedata <- treedata %>% drop_na(`Tree#`)

# R and I both get easily confused about the % slope column so I'm renaming it

treedata <- treedata %>% rename("PercentSlope" = `% slope`)

# some of the trees were measured with horizontal distance, some with slope + slope distance, some with both. we want all trees to have horizontal distance. 

# create a blank column for horizontal distance

treedata$HorizontalDistance <- 0

# make the percent slope and slope distance columns numeric instead of character

treedata$PercentSlope <- as.numeric(treedata$PercentSlope)

treedata$`Slope distance` <- as.numeric(treedata$`Slope distance`)

# if horizontal distance is NA, then calculate it from percent slope and slope distance, then put that value in the horizontal distance column. if horizontal distance is not NA, use the value the crew measured

for(i in 1:nrow(treedata)) {
  if(is.na(treedata$`H Distance`[i])) {
    treedata[i,]$HorizontalDistance = (treedata[i,]$`Slope distance`)*cos(atan(treedata[i,]$PercentSlope/100))
  }
  else {
    treedata[i,]$HorizontalDistance = treedata[i,]$`H Distance`
  }
}

# convert the plot center coordinates to UTM10N format so the calculations in the next step work. first, merge smallplotdata and largeplotdata into 1 spatial dataframe

# plot center sf objects all have different columns bc of course they do, first need to reformat them so they all have the same columns

L506 <- L506[,-1:-18]

L511 <- L511[,-1:-18]

L512 <- L512[,-1:-18]

L515 <- L515[,-1:-18]

L517 <- L517[,-1:-19]

L525 <- L525[,-1:-18]

S107 <- S107[,-1:-19]

S109 <- S109[,-1:-18]

S113 <- S113[,-1:-18]

S118 <- S118[,-1:-19]

allplots <- rbind(L506, L511, L512, L515, L517, L525, S107, S109, S113, S118) 

allplots_UTM10N <- st_transform (allplots, 32610) 

# save the plot centers together in one spatial data file just in case it's useful later

st_write(allplots, data("Stem_Batch_1_updatedplotcenters.gpkg"),delete_dsn=TRUE)

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

st_write(trees_sp %>% st_transform(4326),data("Stem_Batch_1_treecoordinates_emp.edits.gpkg"),delete_dsn=TRUE)

