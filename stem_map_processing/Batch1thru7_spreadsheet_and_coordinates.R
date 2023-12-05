# Author: Emily Marie Purvis
# Date: 11.2.2023
# Goal: combine Batch 1-7 GPS coordinates into KML and combine Batch 1-7 spreadsheets 

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(stringr)

#### Combine coordinates into one KML ####

#### First, the original "target" coordinates ####

# Load data

largeplot_targets <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_1\\large-plots_for-iri.kml") 

smallplot_targets <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_1\\small-plots_for-iri_v2.kml") 

# check CRS
# st_crs(largeplot_targets)
# 4326

# check CRS
# st_crs(smallplot_targets)
# 4326

# Combine into one spatial dataframe

allplots_target <- rbind(largeplot_targets, smallplot_targets)

# Remove random empty column

allplots_target <- allplots_target [-c(2)]

# Export

st_write(allplots_target, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\alltargetplotcentersWGS84.kml"),delete_dsn=TRUE)

st_write(allplots_target %>% st_transform(32610),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\alltargetplotcentersUTM10N.kml"),delete_dsn=TRUE)

#### Now the updated GPS coordinates for Batches 1-7

#### Batch 1 ####

# Set temporary working directory to save typing

setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_1\\updated (9.11.23) data and output files\\Stem_Batch_1")

# Import plot coordinates

L506 <- read_sf('Post_Processed_GPS\\L-506_g\\L-506.shp') 

L506 = L506 %>% mutate(`Plot#` = 'L506')

L506 <- L506 [-c(1:18, 21:22)]

# st_crs(L506)
# EPSG 4326

L511 <- read_sf('Post_Processed_GPS\\L-511_g_CORRECTED\\L511.shp') 

L511 = L511 %>% mutate(`Plot#` = 'L511')

L511 <- L511 [-c(1:18, 21:22)]

# st_crs(L511)

L512 <- read_sf('Post_Processed_GPS\\L-512_g\\L-512.shp') 

L512 = L512 %>% mutate(`Plot#` = 'L512')

L512 <- L512 [-c(1:18, 21:22)]

# st_crs(L512)

L515 <- read_sf('Post_Processed_GPS\\L-515_g\\L-515.shp')

L515 = L515 %>% mutate(`Plot#` = 'L515')

L515 <- L515 [-c(1:18, 21:22)]

# st_crs(L515)

L517 <- read_sf('Post_Processed_GPS\\L-517_g\\L-517.shp') 

L517 = L517 %>% mutate(`Plot#` = 'L517')

L517 <- L517 [-c(1:19, 22:23)]

# st_crs(L517)

L525 <- read_sf('Post_Processed_GPS\\L-525_g\\L-525.shp') 

L525 = L525 %>% mutate(`Plot#` = 'L525')

L525 <- L525 [-c(1:18, 21:22)]

# st_crs(L525)

S107 <- read_sf('Post_Processed_GPS\\S-107_g\\S-107.shp') 

S107 = S107 %>% mutate(`Plot#` = 'S107')

S107 <- S107 [-c(1:19, 22:23)]

# st_crs(S107)

S109 <- read_sf('Post_Processed_GPS\\S-109_g\\S-109.shp') 

S109 = S109 %>% mutate(`Plot#` = 'S109')

S109 <- S109 [-c(1:18, 21:22)]

# st_crs(S109)

S113 <- read_sf('Post_Processed_GPS\\S-113_g\\S-113.shp') 

S113 = S113 %>% mutate(`Plot#` = 'S113')

S113 <- S113 [-c(1:18, 21:22)]

# st_crs(S113)

S118 <- read_sf('Post_Processed_GPS\\S-118_g\\S-118.shp') 

S118 = S118 %>% mutate(`Plot#` = 'S118')

S118 <- S118 [-c(1:19, 22:23)]

# st_crs(S118)

Batch1plotsWGS84 <- rbind(L506, L511, L512, L515, L517, L525, S107, S109, S113, S118)

Batch1plotsWGS84 <- Batch1plotsWGS84 [-c(1:2)]

st_write(Batch1plotsWGS84, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_1\\updated (9.11.23) data and output files\\Stem_Batch_1_plotcoordinatesWGS84.gpkg"),delete_dsn=TRUE)

Batch1plotsUTM10N <- Batch1plotsWGS84 %>% st_transform(32610)

st_write(Batch1plotsUTM10N, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_1\\updated (9.11.23) data and output files\\Stem_Batch_1_plotcoordinatesUTM10N.gpkg"),delete_dsn=TRUE)

#### Batch 2 ####

# Set temporary working directory to save typing

setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\updated (9.11.23) data and output files\\Stem_Batch_2")

# Import plot coordinates

L502 <- read_sf('Post_Processed_GPS\\L-502_g\\L-502.shp') 

L502 = L502 %>% mutate(`Plot#` = 'L502')

L502 <- L502 [-c(1:23)]

# st_crs(L502)
# EPSG 4326

L504 <- read_sf('Post_Processed_GPS\\L-504_g\\L-504.shp') 

L504 = L504 %>% mutate(`Plot#` = 'L504')

L504 = L504 %>% st_set_crs(4326)

L504 <- L504 [-c(1:22)]

# not all shapefiles have a crs, so I'm checking manually and adding if necessary
# st_crs(L504)

L505 <- read_sf('Post_Processed_GPS\\L-505_g\\L-505.shp') 

L505 = L505 %>% mutate(`Plot#` = 'L505')

L505 = L505 %>% st_set_crs(4326)

L505 <- L505 [-c(1:22)]

# st_crs(L505)

L507 <- read_sf('Post_Processed_GPS\\L-507_g\\L-507.shp') 

L507 = L507 %>% mutate(`Plot#` = 'L507')

L507 <- L507 [-c(1:23)]

# st_crs(L507)

L508 <- read_sf('Post_Processed_GPS\\L-508_g\\L-508.shp') 

L508 = L508 %>% mutate(`Plot#` = 'L508')

L508 <- L508 [-c(1:23)]

# st_crs(L508)

L521 <- read_sf('Post_Processed_GPS\\L-521_g\\L-521.shp') 

L521 = L521 %>% mutate(`Plot#` = 'L521')

L521 = L521 %>% st_set_crs(4326)

L521 <- L521 [-c(1:22)]

# st_crs(L521)

L529 <- read_sf('Post_Processed_GPS\\L-529_g\\L-529.shp') 

L529 = L529 %>% mutate(`Plot#` = 'L529')

L529 <- L529 [-c(1:22)]

# st_crs(L529)

L536 <- read_sf('Post_Processed_GPS\\L-536_g\\L-536.shp') 

L536 = L536 %>% mutate(`Plot#` = 'L536')

L536 <- L536 [-c(1:23)]

# st_crs(L536)

S038 <- read_sf('Post_Processed_GPS\\S-038_g\\S-038.shp') 

S038 = S038 %>% mutate(`Plot#` = 'S038')

S038 <- S038 [-c(1:23)]

# st_crs(S038)

S313 <- read_sf('Post_Processed_GPS\\S-313_g\\S-313.shp') 

S313 = S313 %>% mutate(`Plot#` = 'S313')

S313 <- S313 [-c(1:22)]

# st_crs(S313)

Batch2plotsWGS84 <- rbind(L502, L504, L505, L507, L508, L521, L529, L536, S038, S313)

st_write(Batch2plotsWGS84, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\updated (9.11.23) data and output files\\Stem_Batch_2_plotcoordinatesWGS84.gpkg"),delete_dsn=TRUE)

Batch2plotsUTM10N <- Batch2plotsWGS84 %>% st_transform(32610)

st_write(Batch2plotsUTM10N, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\updated (9.11.23) data and output files\\Stem_Batch_2_plotcoordinatesUTM10N.gpkg"),delete_dsn=TRUE)

#### Batch 3 ####

setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_3\\Post_Processed_GPS\\")

# Import plot coordinates

L519 <- read_sf('L-519_g\\L-519.shp') 

L519 = L519 %>% mutate(`Plot#` = 'L519')

L519 <- L519 [-c(1:19, 22:23)]

# st_crs(L519)
# EPSG 4326

S015 <- read_sf('S-015_g\\S-015.shp') 

S015 = S015 %>% mutate(`Plot#` = 'S015')

S015 <- S015 [-c(1:18, 21:22)]

# st_crs(S015)
# EPSG 4326

S074 <- read_sf('S-074_g\\S_074.shp') 

S074 = S074 %>% mutate(`Plot#` = 'S074')

S074 <- S074 [-c(1:19, 22:23)]

# st_crs(S074)
# EPSG 4326

S075 <- read_sf('S-075_g\\S-075.shp') 

S075 = S075 %>% mutate(`Plot#` = 'S075')

S075 <- S075 [-c(1:19, 22:23)]

# st_crs(S075)
# EPSG 4326

S080 <- read_sf('S-080_g\\S-080.shp') 

S080 = S080 %>% mutate(`Plot#` = 'S080')

S080 <- S080 [-c(1:18, 21:22)]

# st_crs(S080)
# S080 <- S080 %>% st_set_crs(4326)
# EPSG 4326

S083 <- read_sf('S-083_g\\S-083.shp') 

S083 = S083 %>% mutate(`Plot#` = 'S083')

S083 <- S083 [-c(1:18, 21:22)]

# st_crs(S083)
# S083 <- S083 %>% st_set_crs(4326)
# EPSG 4326

S084 <- read_sf('S-084_g\\S-084.shp') 

S084 = S084 %>% mutate(`Plot#` = 'S084')

S084 <- S084 [-c(1:18, 21:22)]

# st_crs(S084)
# S084 <- S084 %>% st_set_crs(4326)
# EPSG 4326

S087 <- read_sf('S-087_g\\S-087.shp') 

S087 = S087 %>% mutate(`Plot#` = 'S087')

S087 <- S087 [-c(1:18, 21:22)]

# st_crs(S087)
# S087 <- S087 %>% st_set_crs(4326)
# EPSG 4326

S097 <- read_sf('C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_3\\rebatch3gpsfiles_s097\\S-097.shp') 

S097 = S097 %>% mutate(`Plot#` = 'S097')

S097 <- S097 [-c(1:19, 22:23)]

# st_crs(S097)
# EPSG 4326

S973 <- read_sf('C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_3\\rebatch3gpsfiles_s973\\S-973.shp')

S973 = S973 %>% mutate(`Plot#` = 'S973')

S973 <- S973 [-c(1:19, 22:23)]

# st_crs(S097)
# EPSG 4326

Batch3plotsWGS84 <- rbind(L519, S015, S074, S075, S080, S083, S084, S087, S097, S973)

st_write(Batch3plotsWGS84, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_3\\Stem_Batch_3_plotcoordinatesWGS84.gpkg"),delete_dsn=TRUE)

Batch3plotsUTM10N <- Batch3plotsWGS84 %>% st_transform(32610)

st_write(Batch3plotsUTM10N, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_3\\Stem_Batch_3_plotcoordinatesUTM10N.gpkg"),delete_dsn=TRUE)

#### Batches 4-7 ####

# Batch 4

Batch4plotsWGS84 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4_updatedplotcentersWGS84.gpkg")

Batch4plotsWGS84 <- Batch4plotsWGS84 [-c(1:2)]

Batch4plotsUTM10N <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4_updatedplotcentersUTM10N.gpkg")

Batch4plotsUTM10N <- Batch4plotsUTM10N [-c(1:2)]

# Batch 5

Batch5plotsWGS84 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Stem_Batch_5_updatedplotcentersWGS84.gpkg")

Batch5plotsWGS84 <- Batch5plotsWGS84 [-c(1:2)]

Batch5plotsUTM10N <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Stem_Batch_5_updatedplotcentersUTM10N.gpkg")

Batch5plotsUTM10N <- Batch5plotsUTM10N [-c(1:2)]

# Batch 6

Batch6plotsWGS84 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Stem_Batch_6_updatedplotcentersWGS84.gpkg")

Batch6plotsWGS84 <- Batch6plotsWGS84 [-c(1:2)]

Batch6plotsUTM10N <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Stem_Batch_6_updatedplotcentersUTM10N.gpkg")

Batch6plotsUTM10N <- Batch6plotsUTM10N [-c(1:2)]

# Batch 7

Batch7plotsWGS84 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Stem_Batch_7_updatedplotcentersWGS84.gpkg")

Batch7plotsWGS84 <- Batch7plotsWGS84 [-c(1:2)]

Batch7plotsUTM10N <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Stem_Batch_7_updatedplotcentersUTM10N.gpkg")

Batch7plotsUTM10N <- Batch7plotsUTM10N [-c(1:2)]

#### Combining all the batches ####

Batch1plotsWGS84 <- Batch1plotsWGS84 %>% rename (geom=geometry)

Batch2plotsWGS84 <- Batch2plotsWGS84 %>% rename (geom=geometry)

combinedplotsWGS84 <- rbind(Batch1plotsWGS84, Batch2plotsWGS84, Batch4plotsWGS84, Batch5plotsWGS84, Batch6plotsWGS84, Batch7plotsWGS84)

Batch1plotsUTM10N <- Batch1plotsUTM10N %>% rename (geom=geometry)

Batch2plotsUTM10N <- Batch2plotsUTM10N %>% rename (geom=geometry)

combinedplotsUTM10N <- rbind(Batch1plotsUTM10N, Batch2plotsUTM10N, Batch4plotsUTM10N, Batch5plotsUTM10N, Batch6plotsUTM10N, Batch7plotsUTM10N)

st_write(combinedplotsWGS84, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\allupdatedplotcentersWGS84.kml"),delete_dsn=TRUE)

st_write(combinedplotsUTM10N, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\allupdatedplotcentersUTM10N.kml"),delete_dsn=TRUE)

#### Coming back later to combine batch 3 with the other batches ####

combinedplotsWGS84 <- read_sf ("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\allupdatedplotcentersWGS84.kml")

Batch3plotsWGS84 <- Batch3plotsWGS84 [-c(1:2)]

combinedplotsWGS84 <- combinedplotsWGS84 [-c(2)]

Batch3plotsWGS84 <- Batch3plotsWGS84 %>% rename (Name=`Plot#`)

combinedplotsWGS84 <- rbind(Batch3plotsWGS84, combinedplotsWGS84)

st_write(combinedplotsWGS84, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\allupdatedplotcentersWGS84.kml"),delete_dsn=TRUE)

#### Combine Batches 1-7 spreadsheets ####

# Import data

Batch1trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_1\\updated (9.11.23) data and output files\\Stem_Batch_1\\Plot_Data\\StemData_Batch1_emp_edits.xlsx"),sheet=1,col_names = TRUE)

Batch2trees <- read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\updated (9.11.23) data and output files\\Stem_Batch_2\\Plot_Data\\StemData_Batch2.xlsx"),sheet=1,col_names = TRUE)

Batch3trees <- read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_3\\StemData_Batch3.xlsx"),sheet=1,col_names = TRUE)

Batch4trees <- read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Plot_Data\\StemData_Batch4.xlsx"),sheet=1,col_names = TRUE)

Batch5trees <- read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Plot_Data\\Stem_Batch5.xlsx"),sheet=1,col_names = TRUE)

Batch6trees <- read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Plot_Data\\StemData_Batch6.xlsx"),sheet=1,col_names = TRUE)

Batch7trees <- read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Plot_Data\\Stem_Batch7.xlsx"),sheet=1,col_names = TRUE)

# Add batch number to each spreadsheet just in case this info is helpful later

Batch1trees$Batch <- 1
Batch2trees$Batch <- 2
Batch3trees$Batch <- 3
Batch4trees$Batch <- 4
Batch5trees$Batch <- 5
Batch6trees$Batch <- 6
Batch7trees$Batch <- 7

# Rename columns so they all match

Batch4trees <- Batch4trees %>% rename(`Slope distance`=`Slope Dist`, `H Distance`= `H Dist`)

Batch3trees <- Batch3trees %>% rename(`Tree#`=`Tree #`)

Batch5trees <- Batch5trees %>% rename(`Plot#`=`Plot #`)

Batch6trees <- Batch6trees %>% rename(`Plot#`=`Plot #`)

Batch7trees <- Batch7trees %>% rename(`Plot#`=`Plot #`)

#### Combine datasets into 1 ####

combinedtrees <- rbind(Batch1trees, Batch2trees, Batch3trees, Batch4trees, Batch5trees, Batch6trees, Batch7trees)

# Rename columns to be more descriptive

combinedtrees <- combinedtrees %>% rename("PercentSlope" = `% slope`)

combinedtrees <- combinedtrees %>% rename(`Horizontal distance` = `H Distance`)

combinedtrees <- combinedtrees %>% rename(`Horizontal distance (feet)` = `Horizontal distance`)

combinedtrees <- combinedtrees %>% rename(`Slope distance (feet)` = `Slope distance`)

combinedtrees <- combinedtrees %>% rename(`Azimuth` = `AZM`)

combinedtrees <- combinedtrees %>% rename(`DBH (inches)` = `DBH`)

combinedtrees <- combinedtrees %>% rename(`Height (feet)` = `Actual HT`)

combinedtrees <- combinedtrees %>% rename(`CanopyPosition` = `Position`)

# need to delete random rows of all NAs in treedata

combinedtrees <- combinedtrees %>% drop_na(`Plot#`)
combinedtrees <- combinedtrees %>% drop_na(`Tree#`)

#### Add a complete horizontal distance column ####

# some of the trees were measured with horizontal distance, some with slope + slope distance, some with both. we want all trees to have horizontal distance. 

# create a blank column for horizontal distance

combinedtrees$`All Horizontal Distances (feet)` <- 0

# if horizontal distance is NA, then calculate it from percent slope and slope distance, then put that value in the horizontal distance column. if horizontal distance is not NA, use the value the crew measured

combinedtrees$`Slope distance (feet)` <- as.numeric(combinedtrees$`Slope distance (feet)`)

combinedtrees$`PercentSlope` <- as.numeric(combinedtrees$`PercentSlope`)

for(i in 1:nrow(combinedtrees)) {
  if(is.na(combinedtrees$`Horizontal distance (feet)`[i])) {
    combinedtrees[i,]$`All Horizontal Distances (feet)` = (combinedtrees[i,]$`Slope distance (feet)`)*cos(atan(combinedtrees[i,]$`PercentSlope`/100))
  }
  else {
    combinedtrees[i,]$`All Horizontal Distances (feet)` = combinedtrees[i,]$`Horizontal distance (feet)`
  }
}

#### Add plot coordinates to tree dataframe ####

# Import plot data

updatedplotcentersWGS84 <- read_sf ("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\allupdatedplotcentersWGS84.kml")

updatedplotcentersUTM10N <- updatedplotcentersWGS84 %>% st_transform(32610)

targetplotcentersWGS84 <- read_sf ("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\alltargetplotcentersWGS84.kml")

targetplotcentersUTM10N <- targetplotcentersWGS84 %>% st_transform(32610)

updatedplotcentersWGS84 <- updatedplotcentersWGS84 [-c(2)]

updatedplotcentersUTM10N <- updatedplotcentersUTM10N [-c(2)]

targetplotcentersWGS84 <- targetplotcentersWGS84 [-c(2)]

targetplotcentersUTM10N <- targetplotcentersUTM10N [-c(2)]

updatedplotcentersWGS84 <- updatedplotcentersWGS84 %>% rename (`Plot#`=Name)

updatedplotcentersUTM10N <- updatedplotcentersUTM10N %>% rename (`Plot#`=Name)

targetplotcentersWGS84 <- targetplotcentersWGS84 %>% rename (`Plot#`=Name)

targetplotcentersUTM10N <- targetplotcentersUTM10N %>% rename (`Plot#`=Name)

# extract the geometry features into columns of lat and long

# extract coordinates

updatedplotcentersWGS84_coordinates <- data.frame(updatedplotcentersWGS84$`Plot#`, st_coordinates(updatedplotcentersWGS84[,1], st_coordinates(updatedplotcentersWGS84[,2]))) 

updatedplotcentersUTM10N_coordinates <- data.frame(updatedplotcentersUTM10N$`Plot#`, st_coordinates(updatedplotcentersUTM10N[,1], st_coordinates(updatedplotcentersUTM10N[,2]))) 

targetplotcentersWGS84_coordinates <- data.frame(targetplotcentersWGS84$`Plot#`, st_coordinates(targetplotcentersWGS84[,1], st_coordinates(targetplotcentersWGS84[,2]))) 

targetplotcentersUTM10N_coordinates <- data.frame(targetplotcentersUTM10N$`Plot#`, st_coordinates(targetplotcentersUTM10N[,1], st_coordinates(targetplotcentersUTM10N[,2]))) 

# rename new columns

updatedplotcentersWGS84_coordinates <- updatedplotcentersWGS84_coordinates %>% rename (`Plot#`=updatedplotcentersWGS84..Plot.., PlotLongitudeWGS84=X, PlotLatitudeWGS84=Y) 

updatedplotcentersWGS84_coordinates <- updatedplotcentersWGS84_coordinates [-c(4)]

updatedplotcentersWGS84_coordinates <- updatedplotcentersWGS84_coordinates %>% rename (UpdatedPlotLongitudeWGS84=PlotLongitudeWGS84, UpdatedPlotLatitudeWGS84=PlotLatitudeWGS84)

updatedplotcentersUTM10N_coordinates <- updatedplotcentersUTM10N_coordinates %>% rename (`Plot#`=updatedplotcentersUTM10N..Plot.., PlotLongitudeUTM10N=X, PlotLatitudeUTM10N=Y) 

updatedplotcentersUTM10N_coordinates <- updatedplotcentersUTM10N_coordinates [-c(4)]

updatedplotcentersUTM10N_coordinates <- updatedplotcentersUTM10N_coordinates %>% rename (UpdatedPlotLongitudeUTM10N=PlotLongitudeUTM10N, UpdatedPlotLatitudeUTM10N=PlotLatitudeUTM10N)

targetplotcentersWGS84_coordinates <- targetplotcentersWGS84_coordinates %>% rename (`Plot#`=targetplotcentersWGS84..Plot.., PlotLongitudeWGS84=X, PlotLatitudeWGS84=Y) 

targetplotcentersWGS84_coordinates <- targetplotcentersWGS84_coordinates [-c(4)]

targetplotcentersWGS84_coordinates <- targetplotcentersWGS84_coordinates %>% rename (TargetPlotLongitudeWGS84=PlotLongitudeWGS84, TargetPlotLatitudeWGS84=PlotLatitudeWGS84)

targetplotcentersUTM10N_coordinates <- targetplotcentersUTM10N_coordinates %>% rename (`Plot#`=targetplotcentersUTM10N..Plot.., PlotLongitudeUTM10N=X, PlotLatitudeUTM10N=Y) 

targetplotcentersUTM10N_coordinates <- targetplotcentersUTM10N_coordinates [-c(4)]

targetplotcentersUTM10N_coordinates <- targetplotcentersUTM10N_coordinates %>% rename (TargetPlotLongitudeUTM10N=PlotLongitudeUTM10N, TargetPlotLatitudeUTM10N=PlotLatitudeUTM10N)

# make sure all plot names are in the same format

combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-506'] <- 'L506'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-511'] <- 'L511'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-512'] <- 'L512'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-515'] <- 'L515'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-517'] <- 'L517'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-525'] <- 'L525'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-107'] <- 'S107'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-109'] <- 'S109'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-113'] <- 'S113'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-118'] <- 'S118'

combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-521'] <- 'L521'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-505'] <- 'L505'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-504'] <- 'L504'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-536'] <- 'L536'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-507'] <- 'L507'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-502'] <- 'L502'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-038'] <- 'S038'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-529'] <- 'L529'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-508'] <- 'L508'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-313'] <- 'S313'

combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-519'] <- 'L519'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-087'] <- 'S087'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-084'] <- 'S084'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-080'] <- 'S080'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-083'] <- 'S083'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-097'] <- 'S097'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-038'] <- 'S038'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-075'] <- 'S075'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-973'] <- 'S973'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-015'] <- 'S015'

combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-522'] <- 'L522'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-528'] <- 'L528'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-091'] <- 'S091'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-907'] <- 'S907'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-524'] <- 'L524'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-520'] <- 'L520'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-305'] <- 'S305'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-535'] <- 'L535'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-539'] <- 'L539'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-534'] <- 'L534'

combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'L-092'] <- 'L092'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-100'] <- 'S100'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-103'] <- 'S103'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-099'] <- 'S099'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-105'] <- 'S105'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-111'] <- 'S111'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-098'] <- 'S098'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-101'] <- 'S101'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-108'] <- 'S108'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-106'] <- 'S106'

combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-119'] <- 'S119'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-965'] <- 'S965'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-104'] <- 'S104'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-110'] <- 'S110'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-112'] <- 'S112'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-114'] <- 'S114'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-115'] <- 'S115'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-117'] <- 'S117'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-120'] <- 'S120'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-503'] <- 'S503'

combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-303'] <- 'S303'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-931'] <- 'S931'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-037'] <- 'S037'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-058'] <- 'S058'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-065'] <- 'S065'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-066'] <- 'S066'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-070'] <- 'S070'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-076'] <- 'S076'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-989'] <- 'S989'
combinedtrees$`Plot#`[combinedtrees$`Plot#` == 'S-071'] <- 'S071'

# merge into new data frames

allupdatedplotcoordinates <- full_join(updatedplotcentersWGS84_coordinates, updatedplotcentersUTM10N_coordinates, by="Plot#")

alltargetplotcoordinates <- full_join(targetplotcentersWGS84_coordinates, targetplotcentersUTM10N_coordinates, by="Plot#")

allplotcoordinates <- full_join (allupdatedplotcoordinates, alltargetplotcoordinates, by="Plot#")

# merge into tree data frame

combinedtrees <- full_join(combinedtrees, allplotcoordinates, by="Plot#")

# get rid of plots that haven't had data collected on them yet

combinedtrees <- combinedtrees %>% drop_na(`Tree#`)

#### Tree coordinates ####

# Make new columns for tree coordinates

combinedtrees$`TreeLongitudeUTM10N` <- 0

combinedtrees$`TreeLatitudeUTM10N` <- 0

##### Calculate tree coordinates in UTMs ####

# Longitude UTM10N

for(i in 1:nrow(combinedtrees)) {
  if(combinedtrees[i,]$Batch == 3) { 
    combinedtrees[i,]$`TreeLongitudeUTM10N` = (combinedtrees[i,]$TargetPlotLongitudeUTM10N) + ((sin(deg2rad(combinedtrees[i,]$Azimuth))) * ((combinedtrees[i,]$`All Horizontal Distances (feet)`) * 0.3048))
  }
  else {
    combinedtrees[i,]$`TreeLongitudeUTM10N` = (combinedtrees[i,]$UpdatedPlotLongitudeUTM10N) + ((sin(deg2rad(combinedtrees[i,]$Azimuth))) * ((combinedtrees[i,]$`All Horizontal Distances (feet)`) * 0.3048))
  }
}

# Latitude UTM10N

for(i in 1:nrow(combinedtrees)) {
  if(combinedtrees[i,]$Batch == 3) { 
    combinedtrees[i,]$`TreeLatitudeUTM10N` = (combinedtrees[i,]$TargetPlotLatitudeUTM10N) + ((cos(deg2rad(combinedtrees[i,]$Azimuth))) * ((combinedtrees[i,]$`All Horizontal Distances (feet)`) * 0.3048))
  }
  else {
    combinedtrees[i,]$`TreeLatitudeUTM10N` = (combinedtrees[i,]$UpdatedPlotLatitudeUTM10N) + ((cos(deg2rad(combinedtrees[i,]$Azimuth))) * ((combinedtrees[i,]$`All Horizontal Distances (feet)`) * 0.3048))
  }
}

#### Convert to WGS84 Coordinates ####

# give trees ID numbers

combinedtrees = combinedtrees %>% mutate(tree_id = 1:nrow(combinedtrees))

# remove trees without lat/lon --> this happens when there is no horizontal distance AND either/or no percent slope/slope distance, so a horizontal distance cannot be calculated

# fortunately there are only six trees in this whole dataset of 4969 that don't have a horizontal distance, five in batch 2 (an ABCO in plot L536, an ABCO in plot L502, two LIDE3 and one ACMA3 in L529) and one in batch 4 (a PILA in plot L535)

combinedtrees <- combinedtrees %>% drop_na(`TreeLongitudeUTM10N`)

# make a spatial data frame

treecoordinatesconversion <- data.frame(combinedtrees$tree_id, combinedtrees$TreeLongitudeUTM10N, combinedtrees$TreeLatitudeUTM10N) 

treecoordinatesconversion <- treecoordinatesconversion %>% rename (tree_id=combinedtrees.tree_id, TreeLongitudeUTM10N=combinedtrees.TreeLongitudeUTM10N, TreeLatitudeUTM10N=combinedtrees.TreeLatitudeUTM10N)

treecoordinatesconversion <- st_as_sf(treecoordinatesconversion, coords = c("TreeLongitudeUTM10N", "TreeLatitudeUTM10N"), crs = 32610, remove=F)

# change the CRS

treecoordinatesconversion <- treecoordinatesconversion %>% st_transform(4326)

# save this in case it's helpful later

st_write(treecoordinatesconversion, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\alltreecoordinatesWGS84.kml"),delete_dsn=TRUE)

# extract the geometry features into columns of lat and long WGS84, then merge back into the tree data frame

treecoordinatesconversionWGS84 <- data.frame(treecoordinatesconversion$tree_id, st_coordinates(treecoordinatesconversion[,1], st_coordinates(treecoordinatesconversion[,2]))) 

treecoordinatesconversionWGS84 <- treecoordinatesconversionWGS84 %>% rename (tree_id=treecoordinatesconversion.tree_id, TreeLongitudeWGS84=X, TreeLatitudeWGS84=Y)

combinedtrees <- full_join (combinedtrees, treecoordinatesconversionWGS84, by="tree_id")

#### Convert DBH from inches to cm ####

# add new column

combinedtrees$`DBH (cm)` <- 0

# calculate 

combinedtrees$`DBH (cm)` = combinedtrees$`DBH (inches)` * 2.54

#### Convert species codes ####

combinedtrees$`Species`[combinedtrees$`Species` == '122'] <- 'PIPO'
combinedtrees$`Species`[combinedtrees$`Species` == '15'] <- 'ABCO'
combinedtrees$`Species`[combinedtrees$`Species` == '20'] <- 'ABMA'
combinedtrees$`Species`[combinedtrees$`Species` == '117'] <- 'PILA'
combinedtrees$`Species`[combinedtrees$`Species` == '101'] <- 'PIAL'
combinedtrees$`Species`[combinedtrees$`Species` == '119'] <- 'PIMO3'
combinedtrees$`Species`[combinedtrees$`Species` == '108'] <- 'PICOL'
combinedtrees$`Species`[combinedtrees$`Species` == '81'] <- 'CADE27'
combinedtrees$`Species`[combinedtrees$`Species` == '202'] <- 'PSME'
combinedtrees$`Species`[combinedtrees$`Species` == '127'] <- 'PISA2'
combinedtrees$`Species`[combinedtrees$`Species` == '116'] <- 'PIJE'
combinedtrees$`Species`[combinedtrees$`Species` == '103'] <- 'PIAT'
combinedtrees$`Species`[combinedtrees$`Species` == '361'] <- 'ARME'
combinedtrees$`Species`[combinedtrees$`Species` == '631'] <- 'LIDE3'
combinedtrees$`Species`[combinedtrees$`Species` == '312'] <- 'ACMA3' # Bigleaf Maple
combinedtrees$`Species`[combinedtrees$`Species` == '981'] <- 'UMCA'
combinedtrees$`Species`[combinedtrees$`Species` == '333'] <- 'AECA'
combinedtrees$`Species`[combinedtrees$`Species` == '805'] <- 'QUCH2'
combinedtrees$`Species`[combinedtrees$`Species` == '807'] <- 'QUDO'
combinedtrees$`Species`[combinedtrees$`Species` == '818'] <- 'QUKE'
combinedtrees$`Species`[combinedtrees$`Species` == '839'] <- 'QUWI2'
combinedtrees$`Species`[combinedtrees$`Species` == '64'] <- 'JUOC'
combinedtrees$`Species`[combinedtrees$`Species` == '768'] <- 'PREM'
combinedtrees$`Species`[combinedtrees$`Species` == '21'] <- 'ABMAS' # Shasta Red Fir
combinedtrees$`Species`[combinedtrees$`Species` == '313'] <- 'ACNE2' # Box Elder
combinedtrees$`Species`[combinedtrees$`Species` == '492'] <- 'CONU4'
combinedtrees$`Species`[combinedtrees$`Species` == '999'] <- 'Unknown'
combinedtrees$`Species`[combinedtrees$`Species` == '9'] <- 'Unknown'
combinedtrees$`Species`[is.na(combinedtrees$`Species`)] <- "Unknown"

#### Convert damage codes ####

combinedtrees$`Live Tree Defects`[combinedtrees$`Live Tree Defects` == '0'] <- 'no damage'

combinedtrees$`Live Tree Defects`[is.na(combinedtrees$`Live Tree Defects`)] <- ""

combinedtrees$`Live Tree Defects` <- str_replace(combinedtrees$`Live Tree Defects`,  "90001", "broken top")

combinedtrees$`Live Tree Defects` <- str_replace(combinedtrees$`Live Tree Defects`, "90002", "dead top")

combinedtrees$`Live Tree Defects` <- str_replace(combinedtrees$`Live Tree Defects`, "90004", "forked top")

combinedtrees$`Live Tree Defects` <- str_replace(combinedtrees$`Live Tree Defects`, "90005", "forked below merch top")

combinedtrees$`Live Tree Defects` <- str_replace(combinedtrees$`Live Tree Defects`, "90006", "crook or sweep")

combinedtrees$`Live Tree Defects` <- str_replace(combinedtrees$`Live Tree Defects`, "9006", "crook or sweep")

combinedtrees$`Live Tree Defects` <- str_replace(combinedtrees$`Live Tree Defects`, "90011", "open wound")

#### Convert status into live/dead ####

combinedtrees$`Live/Dead`[combinedtrees$`Live/Dead` == '1'] <- 'Live'

combinedtrees$`Live/Dead`[combinedtrees$`Live/Dead` == '11'] <- 'Live' # an obvious typo

combinedtrees$`Live/Dead`[combinedtrees$`Live/Dead` == '18.2'] <- 'Live' # just one of these, this is the DBH in inches, has no decay class

combinedtrees$`Live/Dead`[combinedtrees$`Live/Dead` == '3'] <- 'Live' # just one of these, has no decay class

combinedtrees$`Live/Dead`[combinedtrees$`Live/Dead` == '2'] <- 'Dead'

#### Convert canopy position into words ####

combinedtrees$`CanopyPosition`[combinedtrees$`CanopyPosition` == '1'] <- 'Open Grown'
combinedtrees$`CanopyPosition`[combinedtrees$`CanopyPosition` == '2'] <- 'Dominant'
combinedtrees$`CanopyPosition`[combinedtrees$`CanopyPosition` == '3'] <- 'Co-dominant'
combinedtrees$`CanopyPosition`[combinedtrees$`CanopyPosition` == '4'] <- 'Intermediate'
combinedtrees$`CanopyPosition`[combinedtrees$`CanopyPosition` == '5'] <- 'Overtopped'

combinedtrees$`CanopyPosition`[combinedtrees$`CanopyPosition` == '12'] <- ''
combinedtrees$`CanopyPosition`[combinedtrees$`CanopyPosition` == '35'] <- ''

#### Export ####

write.csv(combinedtrees, "C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\Batch1thru7trees.csv")
