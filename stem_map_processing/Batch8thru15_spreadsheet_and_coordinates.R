# Author: Emily Marie Purvis
# Date: 12.5.2023
# Goal: combine Batch 8-15 GPS coordinates into KML and combine Batch 1-7 spreadsheets 

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(stringr)
library(pracma)

#### Combine updated coordinates for all the batches into one KML ####

# Load batches 1-7 data

plots1thru7WGS84 <- read_sf ("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\allupdatedplotcentersWGS84.kml")

#### Now the updated GPS coordinates for Batches 8-15 ####

# Set temporary working directory to save typing

setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization")

#### Batch 8 ####

# Import plot coordinates

S023 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S023\\S023.shp") 

S023= S023 %>% mutate(Name = 'S023')

S023 <- S023 [-c(1:19, 22:23)]

# st_crs(S023)
# EPSG 4326

S028 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S028\\S028.shp") 

S028= S028 %>% mutate(Name = 'S028')

S028 <- S028 [-c(1:19, 22:23)]

# st_crs(S028)
# EPSG 4326

S032 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S032\\S032.shp") 

S032= S032 %>% mutate(Name = 'S032')

S032 <- S032 [-c(1:19, 22:23)]

# st_crs(S032)
# EPSG 4326

S079 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S079\\S079.shp") 

S079= S079 %>% mutate(Name = 'S079')

S079 <- S079 [-c(1:19, 22:23)]

# st_crs(S079)
# EPSG 4326

S302 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S302\\S302.shp") 

S302= S302 %>% mutate(Name = 'S302')

S302 <- S302 [-c(1:18, 21:22)]

# st_crs(S302)
# EPSG 4326

S308 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S308\\S308.shp") 

S308= S308 %>% mutate(Name = 'S308')

S308 <- S308 [-c(1:18, 21:22)]

# st_crs(S308)
# EPSG 4326

S310 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S310\\S310.shp") 

S310= S310 %>% mutate(Name = 'S310')

S310 <- S310 [-c(1:19, 22:23)]

# st_crs(S310)
# EPSG 4326

S903 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S-903\\S-903.shp") 

S903= S903 %>% mutate(Name = 'S903')

S903 <- S903 [-c(1:19, 22:23)]

# st_crs(S903)
# EPSG 4326

S945 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S945\\S945.shp") 

S945= S945 %>% mutate(Name = 'S945')

S945 <- S945 [-c(1:19, 22:23)]

# st_crs(S945)
# EPSG 4326

S960 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S-960\\S-960.shp") 

S960= S960 %>% mutate(Name = 'S960')

S960 <- S960 [-c(1:19, 22:23)]

# st_crs(S960)
# EPSG 4326

# Combine all plots

Batch8plotsWGS84 <- rbind(S023, S028, S032, S079, S302, S308, S310, S903, S945, S960)

Batch8plotsWGS84 <- Batch8plotsWGS84 [-c(1:2)]

st_write(Batch8plotsWGS84, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_8\\Stem_Batch_8_plotcoordinatesWGS84.gpkg"),delete_dsn=TRUE)

Batch8plotsUTM10N <- Batch8plotsWGS84 %>% st_transform(32610)

st_write(Batch8plotsUTM10N, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_8\\Stem_Batch_8_plotcoordinatesUTM10N.gpkg"),delete_dsn=TRUE)

#### Batch 9 ####

# Import plot coordinates

S009 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S009\\S009.shp") 

S009= S009 %>% mutate(Name = 'S009')

S009 <- S009 [-c(1:19, 22:23)]

# st_crs(S009)
# EPSG 4326

S030 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S030\\S030.shp") 

S030= S030 %>% mutate(Name = 'S030')

S030 <- S030 [-c(1:19, 22:23)]

# st_crs(S030)
# EPSG 4326

S040 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S040\\S040.shp") 

S040= S040 %>% mutate(Name = 'S040')

S040 <- S040 [-c(1:19, 22:23)]

# st_crs(S040)
# EPSG 4326

S048 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S048\\S048.shp") 

S048= S048 %>% mutate(Name = 'S048')

S048 <- S048 [-c(1:19, 22:23)]

# st_crs(S048)
# EPSG 4326

S053 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S053\\S053.shp") 

S053= S053 %>% mutate(Name = 'S053')

S053 <- S053 [-c(1:19, 22:23)]

# st_crs(S053)
# EPSG 4326

S062 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S062\\S062.shp") 

S062= S062 %>% mutate(Name = 'S062')

S062 <- S062 [-c(1:19, 22:23)]

# st_crs(S062)
# EPSG 4326

S063 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S063\\S063.shp") 

S063= S063 %>% mutate(Name = 'S063')

S063 <- S063 [-c(1:19, 22:23)]

# st_crs(S063)
# EPSG 4326

S073 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S073\\S073.shp") 

S073= S073 %>% mutate(Name = 'S073')

S073 <- S073 [-c(1:19, 22:23)]

# st_crs(S073)
# EPSG 4326

S085 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S085\\S085.shp") 

S085= S085 %>% mutate(Name = 'S085')

S085 <- S085 [-c(1:19, 22:23)]

# st_crs(S085)
# EPSG 4326

S092 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S092\\S092.shp") 

S092= S092 %>% mutate(Name = 'S092')

S092 <- S092 [-c(1:19, 22:23)]

# st_crs(S092)
# EPSG 4326

# Combine all plots

Batch9plotsWGS84 <- rbind(S009, S030, S040, S048, S053, S062, S063, S073, S085, S092)

Batch9plotsWGS84 <- Batch9plotsWGS84 [-c(1:2)]

st_write(Batch9plotsWGS84, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_9\\Stem_Batch_9_plotcoordinatesWGS84.gpkg"),delete_dsn=TRUE)

Batch9plotsUTM10N <- Batch9plotsWGS84 %>% st_transform(32610)

st_write(Batch9plotsUTM10N, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_9\\Stem_Batch_9_plotcoordinatesUTM10N.gpkg"),delete_dsn=TRUE)

#### Batch 10 ####

# Import plot coordinates

S036 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S036\\S036.shp")

S036= S036 %>% mutate(Name = 'S036')

S036 <- S036 [-c(1:19, 22:23)]

# st_crs(S036)
# EPSG 4326

S042 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S042\\S042.shp")

S042= S042 %>% mutate(Name = 'S042')

S042 <- S042 [-c(1:19, 22:23)]

# st_crs(S042)
# EPSG 4326

S046 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S046\\S046.shp")

S046= S046 %>% mutate(Name = 'S046')

S046 <- S046 [-c(1:19, 22:23)]

# st_crs(S046)
# EPSG 4326

S051 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S051\\S051.shp")

S051= S051 %>% mutate(Name = 'S051')

S051 <- S051 [-c(1:19, 22:23)]

# st_crs(S051)
# EPSG 4326

S055 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S055\\S055.shp")

S055= S055 %>% mutate(Name = 'S055')

S055 <- S055 [-c(1:19, 22:23)]

# st_crs(S055)
# EPSG 4326

S089 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S089\\S089.shp")

S089= S089 %>% mutate(Name = 'S089')

S089 <- S089 [-c(1:19, 22:23)]

# st_crs(S089)
# EPSG 4326

S093 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S093\\S093.shp")

S093= S093 %>% mutate(Name = 'S093')

S093 <- S093 [-c(1:19, 22:23)]

# st_crs(S093)
# EPSG 4326

S314 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S314\\S314.shp")

S314= S314 %>% mutate(Name = 'S314')

S314 <- S314 [-c(1:19, 22:23)]

# st_crs(S314)
# EPSG 4326

S315 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S315\\S315.shp")

S315= S315 %>% mutate(Name = 'S315')

S315 <- S315 [-c(1:19, 22:23)]

# st_crs(S315)
# EPSG 4326

S982 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S982\\S982.shp")

S982= S982 %>% mutate(Name = 'S982')

S982 <- S982 [-c(1:19, 22:23)]

# st_crs(S982)
# EPSG 4326

# Combine all plots

Batch10plotsWGS84 <- rbind(S036, S042, S046, S051, S055, S089, S093, S314, S315, S982)

Batch10plotsWGS84 <- Batch10plotsWGS84 [-c(1:2)]

st_write(Batch10plotsWGS84, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_10\\Stem_Batch_10_plotcoordinatesWGS84.gpkg"),delete_dsn=TRUE)

Batch10plotsUTM10N <- Batch10plotsWGS84 %>% st_transform(32610)

st_write(Batch10plotsUTM10N, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_10\\Stem_Batch_10_plotcoordinatesUTM10N.gpkg"),delete_dsn=TRUE)

#### Batch 11 ####

# Import plot coordinates

S001 <- read_sf("TNC.Stem_Batch_11\\Batch_11_Final\\S001\\S001.shp") 

S001= S001 %>% mutate(Name = 'S001')

S001 <- S001 [-c(1:19, 22:23)]

# st_crs(S001)
# EPSG 4326

S007 <- read_sf("TNC.Stem_Batch_11\\Batch_11_Final\\S007\\S007.shp") 

S007= S007 %>% mutate(Name = 'S007')

S007 <- S007 [-c(1:19, 22:23)]

# st_crs(S007)
# EPSG 4326

S008 <- read_sf("TNC.Stem_Batch_11\\Batch_11_Final\\S008\\S008.shp") 

S008= S008 %>% mutate(Name = 'S008')

S008 <- S008 [-c(1:19, 22:23)]

# st_crs(S008)
# EPSG 4326

S058<- read_sf("TNC.Stem_Batch_11\\Batch_11_Final\\S058\\S058.shp") # repeat from another batch??

S058= S058 %>% mutate(Name = 'S058')

S058 <- S058 [-c(1:19, 22:23)]

# st_crs(S058)
# EPSG 4326

S065 <- read_sf("TNC.Stem_Batch_11\\Batch_11_Final\\S065\\S065.shp") 

S065= S065 %>% mutate(Name = 'S065')

S065 <- S065 [-c(1:19, 22:23)]

# st_crs(S065)
# EPSG 4326

S070 <- read_sf("TNC.Stem_Batch_11\\Batch_11_Final\\S070\\S070.shp") 

S070= S070 %>% mutate(Name = 'S070')

S070 <- S070 [-c(1:19, 22:23)]

# st_crs(S070)
# EPSG 4326

S076 <- read_sf("TNC.Stem_Batch_11\\Batch_11_Final\\S076\\S076.shp") 

S076= S076 %>% mutate(Name = 'S076')

S076 <- S076 [-c(1:19, 22:23)]

# st_crs(S076)
# EPSG 4326

S307 <- read_sf("TNC.Stem_Batch_11\\Batch_11_Final\\S307\\S307.shp") 

S307= S307 %>% mutate(Name = 'S307')

S307 <- S307 [-c(1:19, 22:23)]

# st_crs(S307)
# EPSG 4326

S505 <- read_sf("TNC.Stem_Batch_11\\Batch_11_Final\\S505\\S505.shp") 

S505= S505 %>% mutate(Name = 'S505')

S505 <- S505 [-c(1:19, 22:23)]

# st_crs(S505)
# EPSG 4326

S940 <- read_sf("TNC.Stem_Batch_11\\Batch_11_Final\\S0940\\S940.shp") 

S940= S940 %>% mutate(Name = 'S940')

S940 <- S940 [-c(1:19, 22:23)]

# st_crs(S940)
# EPSG 4326

# Combine all plots

Batch11plotsWGS84 <- rbind(S001, S007, S008, S058, S065, S070, S076, S307, S505, S940)

Batch11plotsWGS84 <- Batch11plotsWGS84 [-c(1:2)]

st_write(Batch11plotsWGS84, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_11\\Stem_Batch_11_plotcoordinatesWGS84.gpkg"),delete_dsn=TRUE)

Batch11plotsUTM10N <- Batch11plotsWGS84 %>% st_transform(32610)

st_write(Batch11plotsUTM10N, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_11\\Stem_Batch_11_plotcoordinatesUTM10N.gpkg"),delete_dsn=TRUE)

#### Batch 12 ####

# Import plot coordinates

S021 <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S021\\S021.shp") 

S021= S021 %>% mutate(Name = 'S021')

S021 <- S021 [-c(1:19, 22:23)]

# st_crs(S021)
# EPSG 4326

S022  <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S022\\S022.shp") 

S022= S022 %>% mutate(Name = 'S022')

S022 <- S022 [-c(1:19, 22:23)]

# st_crs(S022)
# EPSG 4326

S026 <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S-026\\S-026.shp") 

S026= S026 %>% mutate(Name = 'S026')

S026 <- S026 [-c(1:19, 22:23)]

# st_crs(S026)
# EPSG 4326

S034 <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S-034\\S-034.shp") 

S034= S034 %>% mutate(Name = 'S034')

S034 <- S034 [-c(1:19, 22:23)]

# st_crs(S034)
# EPSG 4326

S045 <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S045\\S045.shp") 

S045= S045 %>% mutate(Name = 'S045')

S045 <- S045 [-c(1:19, 22:23)]

# st_crs(S045)
# EPSG 4326

S061 <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S061\\S061.shp") 

S061= S061 %>% mutate(Name = 'S061')

S061 <- S061 [-c(1:19, 22:23)]

# st_crs(S061)
# EPSG 4326

S069 <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S069\\S069.shp") 

S069= S069 %>% mutate(Name = 'S069')

S069 <- S069 [-c(1:19, 22:23)]

# st_crs(S069)
# EPSG 4326

S096 <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S096\\S-096.shp") 

S096= S096 %>% mutate(Name = 'S096')

S096 <- S096 [-c(1:19, 22:23)]

# st_crs(S096)
# EPSG 4326

S507 <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S507\\S507.shp") 

S507= S507 %>% mutate(Name = 'S507')

S507 <- S507 [-c(1:19, 22:23)]

# st_crs(S507)
# EPSG 4326

S508 <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S508\\S-508.shp") 

S508= S508 %>% mutate(Name = 'S508')

S508 <- S508 [-c(1:19, 22:23)]

# st_crs(S508)
# EPSG 4326

# Combine all plots

Batch12plotsWGS84 <- rbind(S021, S022, S026, S034, S045, S061, S069, S096, S507, S508)

Batch12plotsWGS84 <- Batch12plotsWGS84 [-c(1:2)]

st_write(Batch12plotsWGS84, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_12\\Stem_Batch_12_plotcoordinatesWGS84.gpkg"),delete_dsn=TRUE)

Batch12plotsUTM10N <- Batch12plotsWGS84 %>% st_transform(32610)

st_write(Batch12plotsUTM10N, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_12\\Stem_Batch_12_plotcoordinatesUTM10N.gpkg"),delete_dsn=TRUE)

#### Batch 13 ####

# Import plot coordinates

S003 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S003\\S003.shp") 

S003= S003 %>% mutate(Name = 'S003')

S003 <- S003 [-c(1:19, 22:23)]

# st_crs(S003)
# EPSG 4326

S006 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S006\\S-006.shp") 

S006= S006 %>% mutate(Name = 'S006')

S006 <- S006 [-c(1:19, 22:23)]

# st_crs(S006)
# EPSG 4326

S014 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S014\\S014.shp") 

S014= S014 %>% mutate(Name = 'S014')

S014 <- S014 [-c(1:19, 22:23)]

# st_crs(S014)
# EPSG 4326

S044 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S044\\S044.shp") 

S044= S044 %>% mutate(Name = 'S044')

S044 <- S044 [-c(1:19, 22:23)]

# st_crs(S044)
# EPSG 4326

S047 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S047\\S047.shp") 

S047= S047 %>% mutate(Name = 'S047')

S047 <- S047 [-c(1:19, 22:23)]

# st_crs(S047)
# EPSG 4326

S054 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S054\\S054.shp") 

S054= S054 %>% mutate(Name = 'S054')

S054 <- S054 [-c(1:19, 22:23)]

# st_crs(S054)
# EPSG 4326

S312 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S-312\\S-312.shp") 

S312= S312 %>% mutate(Name = 'S312')

S312 <- S312 [-c(1:19, 22:23)]

# st_crs(S312)
# EPSG 4326

S316 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S316\\S316.shp") 

S316= S316 %>% mutate(Name = 'S316')

S316 <- S316 [-c(1:19, 22:23)]

# st_crs(S316)
# EPSG 4326

S502 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S502\\S502.shp") 

S502= S502 %>% mutate(Name = 'S502')

S502 <- S502 [-c(1:19, 22:23)]

# st_crs(S502)
# EPSG 4326

S935 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S935\\S935.shp") 

S935= S935 %>% mutate(Name = 'S935')

S935 <- S935 [-c(1:19, 22:23)]

# st_crs(S935)
# EPSG 4326

# Combine all plots

Batch13plotsWGS84 <- rbind(S003, S006, S014, S044, S047, S054, S312, S316, S502, S935)

Batch13plotsWGS84 <- Batch13plotsWGS84 [-c(1:2)]

st_write(Batch13plotsWGS84, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_13\\Stem_Batch_13_plotcoordinatesWGS84.gpkg"),delete_dsn=TRUE)

Batch13plotsUTM10N <- Batch13plotsWGS84 %>% st_transform(32610)

st_write(Batch13plotsUTM10N, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_13\\Stem_Batch_13_plotcoordinatesUTM10N.gpkg"),delete_dsn=TRUE)

#### Batch 14 ####

# Import plot coordinates

S049 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\049\\Point_generic.shp") 

S049= S049 %>% mutate(Name = 'S049')

S049 <- S049 [-c(1:19, 22:23)]

# st_crs(S049)
# EPSG 4326

S004 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\S-004\\S-004.shp") 

S004= S004 %>% mutate(Name = 'S004')

S004 <- S004 [-c(1:19, 22:23)]

# st_crs(S004)
# EPSG 4326

S010 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\S-010\\S-010.shp") 

S010= S010 %>% mutate(Name = 'S010')

S010 <- S010 [-c(1:19, 22:23)]

# st_crs(S010)
# EPSG 4326

S011 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\S-011\\S-011.shp") 

S011= S011 %>% mutate(Name = 'S011')

S011 <- S011 [-c(1:19, 22:23)]

# st_crs(S011)
# EPSG 4326

S013 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\S013\\S013.shp") 

S013= S013 %>% mutate(Name = 'S013')

S013 <- S013 [-c(1:19, 22:23)]

# st_crs(S013)
# EPSG 4326

S017 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\S017\\S-017.shp") 

S017= S017 %>% mutate(Name = 'S017')

S017 <- S017 [-c(1:19, 22:23)]

# st_crs(S017)
# EPSG 4326

S043 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\S043\\S-043.shp") THERE IS AN ERROR IN THIS SHAPEFILE THERE ARE TWO POINTS

S043= S043 %>% mutate(Name = 'S043')

S043 <- S043 [-c(1:19, 22:23)]

# st_crs(S013)
# EPSG 4326

S086 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\S086\\S086_final.shp") 

S086= S086 %>% mutate(Name = 'S086')

S086 <- S086 [-c(1:19, 22:23)]

# st_crs(S086)
# EPSG 4326

S311 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\S311\\S-311.shp") 

S311= S311 %>% mutate(Name = 'S311')

S311 <- S311 [-c(1:19, 22:23)]

# st_crs(S311)
# EPSG 4326

S506 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\S-506\\S506.shp") 

S506= S506 %>% mutate(Name = 'S506')

S506 <- S506 [-c(1:19, 22:23)]

# st_crs(S506)
# EPSG 4326

# Combine all plots

I also did not do this part because of the fucked up shapefile from earlier








Batch14plotsWGS84 <- rbind(S049, S004, S010, S011, S013, S017, S043, S086, S311, S506)

Batch14plotsWGS84 <- Batch14plotsWGS84 [-c(1:2)]

st_write(Batch14plotsWGS84, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_14\\Stem_Batch_14_plotcoordinatesWGS84.gpkg"),delete_dsn=TRUE)

Batch14plotsUTM10N <- Batch14plotsWGS84 %>% st_transform(32610)

st_write(Batch14plotsUTM10N, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_14\\Stem_Batch_14_plotcoordinatesUTM10N.gpkg"),delete_dsn=TRUE)




#### Batch 15 ####

# Import plot coordinates

S005 <- read_sf("TNC.Stem_Batch_15\\Batch15\\Post_Processed_GPS\\S-005\\S-005.shp") 

S005= S005 %>% mutate(Name = 'S005')

S005 <- S005 [-c(1:19, 22:23)]

# st_crs(S005)
# EPSG 4326

S018 <- read_sf("TNC.Stem_Batch_15\\Batch15\\Post_Processed_GPS\\S-018_g\\S018.shp") 

S018= S018 %>% mutate(Name = 'S018')

S018 <- S018 [-c(1:18, 21:22)]

# st_crs(S018)
# EPSG 4326

S020 <- read_sf("TNC.Stem_Batch_15\\Batch15\\Post_Processed_GPS\\S-020\\S-020.shp") 

S020= S020 %>% mutate(Name = 'S020')

S020 <- S020 [-c(1:19, 22:23)]

# st_crs(S020)
# EPSG 4326

S024 <- read_sf("TNC.Stem_Batch_15\\Batch15\\Post_Processed_GPS\\S-024_g\\Point_generic.shp") 

S024= S024 %>% mutate(Name = 'S024')

S024 <- S024 [-c(1:18, 21:22)]

# st_crs(S024)
# EPSG 4326

S041 <- read_sf("TNC.Stem_Batch_15\\Batch15\\Post_Processed_GPS\\S-041\\S-041.shp") 

S041= S041 %>% mutate(Name = 'S041')

S041 <- S041 [-c(1:19, 22:23)]

# st_crs(S041)
# EPSG 4326

S309 <- read_sf("TNC.Stem_Batch_15\\Batch15\\Post_Processed_GPS\\S-309\\S-309.shp") 

S309= S309 %>% mutate(Name = 'S309')

S309 <- S309 [-c(1:19, 22:23)]

# st_crs(S309)
# EPSG 4326

S501 <- read_sf("TNC.Stem_Batch_15\\Batch15\\Post_Processed_GPS\\S-501\\S-501.shp") 

S501= S501 %>% mutate(Name = 'S501')

S501 <- S501 [-c(1:19, 22:23)]

# st_crs(S501)
# EPSG 4326

# Combine all plots

Batch15plotsWGS84 <- rbind(S005, S018, S020, S024, S041, S309, S501)

Batch15plotsWGS84 <- Batch15plotsWGS84 [-c(1:2)]

st_write(Batch15plotsWGS84, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_15\\Stem_Batch_15_plotcoordinatesWGS84.gpkg"),delete_dsn=TRUE)

Batch15plotsUTM10N <- Batch15plotsWGS84 %>% st_transform(32610)

st_write(Batch15plotsUTM10N, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_15\\Stem_Batch_15_plotcoordinatesUTM10N.gpkg"),delete_dsn=TRUE)

#### Combining all the batches ####

plots1thru7WGS84 <- plots1thru7WGS84 [-c(2)]





I AM DOING THIS WITHOUT BATCH 14, COME BACK AND DO BATCH 14 LATER








THIS IS WHAT I DID







combinedplotsWGS84 <- rbind(plots1thru7WGS84, Batch8plotsWGS84, Batch9plotsWGS84, Batch10plotsWGS84, Batch11plotsWGS84, Batch12plotsWGS84, Batch13plotsWGS84, Batch15plotsWGS84)

combinedplotsUTM10N <- rbind((plots1thru7WGS84 %>% st_transform(32610)), Batch8plotsUTM10N, Batch9plotsUTM10N, Batch10plotsUTM10N, Batch11plotsUTM10N, Batch12plotsUTM10N, Batch13plotsUTM10N, Batch15plotsUTM10N)

st_write(combinedplotsWGS84, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\allupdatedplotcenters_EXCEPTBATCH14_WGS84.kml"),delete_dsn=TRUE)

st_write(combinedplotsUTM10N, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\allupdatedplotcenters_EXCEPTBATCH14_UTM10N.kml"),delete_dsn=TRUE)












THIS IS WHAT I NEED TO COME BACK AND RERUN ONCE BATCH 14 IS AVAILABLE 








combinedplotsWGS84 <- rbind(plots1thru7WGS84, Batch8plotsWGS84, Batch9plotsWGS84, Batch10plotsWGS84, Batch11plotsWGS84, Batch12plotsWGS84, Batch13plotsWGS84, Batch14plotsWGS84, Batch15plotsWGS84)

combinedplotsUTM10N <- rbind((plots1thru7WGS84 %>% st_transform(32610)), Batch8plotsUTM10N, Batch9plotsUTM10N, Batch10plotsUTM10N, Batch11plotsUTM10N, Batch12plotsUTM10N, Batch13plotsUTM10N, Batch14plotsUTM10N, Batch15plotsUTM10N)

st_write(combinedplotsWGS84, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\allupdatedplotcentersWGS84.kml"),delete_dsn=TRUE)

st_write(combinedplotsUTM10N, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\allupdatedplotcentersUTM10N.kml"),delete_dsn=TRUE)












#### Combine Batches 8-15 spreadsheets ####

# Import data

Batch8trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_8\\Batch_8\\Plot_Data\\Stem_Batch_8.xlsx"),sheet=1,col_names = TRUE)

Batch9trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_9\\Batch_9\\Batch_9_Complete.xlsx"),sheet=1,col_names = TRUE)

Batch10trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_10\\Batch_10_final\\Batch_10_Final_Data.xlsx"),sheet=1,col_names = TRUE)

Batch11trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_11\\Batch_11_Final\\Batch_11.xlsx"),sheet=1,col_names = TRUE)

Batch12trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_12\\Batch_12_updated\\Plot_Data\\Stem_Batch12.xlsx"),sheet=1,col_names = TRUE)

Batch13trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\Batch13.xlsx"),sheet=1,col_names = TRUE)

Batch14trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_14\\Batch14\\Plot_Data\\Stem_Batch14.xlsx"),sheet=1,col_names = TRUE)

Batch15trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_15\\Batch15\\Plot_Data\\Stem_Batch15.xlsx"),sheet=1,col_names = TRUE)

# Add batch number to each spreadsheet just in case this info is helpful later

Batch8trees$Batch <- 8
Batch9trees$Batch <- 9
Batch10trees$Batch <- 10
Batch11trees$Batch <- 11
Batch12trees$Batch <- 12
Batch13trees$Batch <- 13
Batch14trees$Batch <- 14
Batch15trees$Batch <- 15

# Rename columns so they all match

Batch11trees <- Batch11trees %>% rename(`Slope distance`=`Slope Dist`, `H Distance`= `H Dist`)

Batch13trees <- Batch13trees %>% rename(`Slope distance`=`Slope Dist`, `H Distance`= `H Dist`)

Batch11trees <- Batch11trees %>% rename(`Plot #`=`Plot#`)

Batch13trees <- Batch13trees %>% rename(`Plot #`=`Plot#`)

#### Combine datasets into 1 ####

combinedtrees <- rbind(Batch8trees, Batch9trees, Batch10trees, Batch11trees, Batch12trees, Batch13trees, Batch14trees, Batch15trees)

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

combinedtrees <- combinedtrees %>% drop_na(`Plot #`)
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

combinedplotsWGS84 <- combinedplotsWGS84 %>% rename (`Plot #`=Name)

combinedplotsUTM10N <- combinedplotsUTM10N %>% rename (`Plot #`=Name)

# extract the geometry features into columns of lat and long

# extract coordinates

combinedplotsWGS84 <- st_zm(combinedplotsWGS84)

combinedplotsWGS84_coordinates <- data.frame(combinedplotsWGS84$`Plot #`, st_coordinates(combinedplotsWGS84[,1], st_coordinates(combinedplotsWGS84[,2]))) 

combinedplotsUTM10N <- st_zm(combinedplotsUTM10N)

combinedplotsUTM10N_coordinates <- data.frame(combinedplotsUTM10N$`Plot #`, st_coordinates(combinedplotsUTM10N[,1], st_coordinates(combinedplotsUTM10N[,2]))) 

# rename new columns

combinedplotsWGS84_coordinates <- combinedplotsWGS84_coordinates %>% rename (`Plot #`=combinedplotsWGS84..Plot..., PlotLongitudeWGS84=X, PlotLatitudeWGS84=Y) 

combinedplotsUTM10N_coordinates <- combinedplotsUTM10N_coordinates %>% rename (`Plot #`=combinedplotsUTM10N..Plot..., PlotLongitudeUTM10N=X, PlotLatitudeUTM10N=Y) 

# make sure all plot names are in the same format

# Batch 8

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-023'] <- 'S023'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-028'] <- 'S028'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-032'] <- 'S032'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-079'] <- 'S079'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-302'] <- 'S302'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-308'] <- 'S308'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-310'] <- 'S310'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-903'] <- 'S903'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-945'] <- 'S945'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-960'] <- 'S960'

# Batch 9

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-009'] <- 'S009'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-030'] <- 'S030'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-040'] <- 'S040'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-048'] <- 'S048'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-053'] <- 'S053'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-062'] <- 'S062'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-063'] <- 'S063'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-073'] <- 'S073'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-085'] <- 'S085'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-092'] <- 'S092'

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 's053'] <- 'S053'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 's063'] <- 'S063'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 's048'] <- 'S048'

# Batch 10

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-036'] <- 'S036'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-042'] <- 'S042'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-046'] <- 'S046'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-051'] <- 'S051'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-055'] <- 'S055'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-089'] <- 'S089'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-093'] <- 'S093'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-314'] <- 'S314'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-315'] <- 'S315'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-982'] <- 'S982'

# Batch 11

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-001'] <- 'S001'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-007'] <- 'S007'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-008'] <- 'S008'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-058'] <- 'S058' # repeat from another batch??
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-065'] <- 'S065'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-070'] <- 'S070'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-076'] <- 'S076'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-307'] <- 'S307'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-505'] <- 'S505'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-940'] <- 'S940'

# Batch 12

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-021'] <- 'S021'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-022'] <- 'S022'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-026'] <- 'S026'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-034'] <- 'S034'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-045'] <- 'S045'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-061'] <- 'S061'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-069'] <- 'S069'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-096'] <- 'S096'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-507'] <- 'S507'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-508'] <- 'S508'

# Batch 13

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-003'] <- 'S003'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-006'] <- 'S006'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-014'] <- 'S014'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-044'] <- 'S044'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-047'] <- 'S047'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-054'] <- 'S054'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-312'] <- 'S312'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-316'] <- 'S316'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-502'] <- 'S502'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-935'] <- 'S935'

# Batch 14

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-049'] <- 'S049'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-004'] <- 'S004'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-010'] <- 'S010'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-011'] <- 'S011'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-013'] <- 'S013'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-017'] <- 'S017'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-043'] <- 'S043'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-086'] <- 'S086'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-311'] <- 'S311'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-506'] <- 'S506'

# Batch 15

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-005'] <- 'S005'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-018'] <- 'S018'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-020'] <- 'S020'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-024'] <- 'S024'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-041'] <- 'S041'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-309'] <- 'S309'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-501'] <- 'S501'

# merge into new data frames

allplotcoordinates <- full_join(combinedplotsWGS84_coordinates, combinedplotsUTM10N_coordinates, by="Plot #") 

# merge into tree data frame

combinedtrees <- full_join(combinedtrees, allplotcoordinates, by="Plot #")

# get rid of plots that haven't had data collected on them yet

combinedtrees <- combinedtrees %>% drop_na(`Tree#`)

#### Tree coordinates ####

# Make new columns for tree coordinates

combinedtrees$`TreeLongitudeUTM10N` <- 0

combinedtrees$`TreeLatitudeUTM10N` <- 0

##### Calculate tree coordinates in UTMs ####






























COME BACK AND DO THIS PART AGAIN WITH BATCH 14. DO EVERYTHING AGAIN.




















# Longitude UTM10N

for(i in 1:nrow(combinedtrees)) {
    combinedtrees[i,]$`TreeLongitudeUTM10N` = (combinedtrees[i,]$PlotLongitudeUTM10N) + ((sin(deg2rad(combinedtrees[i,]$Azimuth))) * ((combinedtrees[i,]$`All Horizontal Distances (feet)`) * 0.3048))
}

# Latitude UTM10N

for(i in 1:nrow(combinedtrees)) {
    combinedtrees[i,]$`TreeLatitudeUTM10N` = (combinedtrees[i,]$PlotLatitudeUTM10N) + ((cos(deg2rad(combinedtrees[i,]$Azimuth))) * ((combinedtrees[i,]$`All Horizontal Distances (feet)`) * 0.3048))
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

st_write(treecoordinatesconversion, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\Batch8thru15_EXCEPTBATCH14_treecoordinatesWGS84.kml"),delete_dsn=TRUE)














SAVE THAT WITH A DIFFERENT FILE NAME THAT INCLUDES BATCH 14











# extract the geometry features into columns of lat and long WGS84, then merge back into the tree data frame

treecoordinatesconversionWGS84 <- data.frame(treecoordinatesconversion$tree_id, st_coordinates(treecoordinatesconversion[,1], st_coordinates(treecoordinatesconversion[,2]))) 

treecoordinatesconversionWGS84 <- treecoordinatesconversionWGS84 %>% rename (tree_id=treecoordinatesconversion.tree_id, TreeLongitudeWGS84=X, TreeLatitudeWGS84=Y)

combinedtrees <- full_join (combinedtrees, treecoordinatesconversionWGS84, by="tree_id")

#### Convert DBH from inches to cm ####





DON'T HAVE TO DO THIS PART AGAIN'












# add new column

combinedtrees$`DBH (cm)` <- 0

# calculate 

combinedtrees$`DBH (cm)` = combinedtrees$`DBH (inches)` * 2.54

#### Convert species codes ####

combinedtrees$`Species`[combinedtrees$`Species` == '122'] <- 'PIPO'
combinedtrees$`Species`[combinedtrees$`Species` == '112'] <- 'PIPO' # 112 is pinus engelmannii, almost certainly a typo
combinedtrees$`Species`[combinedtrees$`Species` == '212'] <- 'PIPO' # 212 is giant sequoia, almost certainly a typo
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
combinedtrees$`Species`[combinedtrees$`Species` == '188'] <- 'QUKE'# another typo I'm guessing at the right answer for-- 188 isn't a code for anything
combinedtrees$`Species`[combinedtrees$`Species` == '816'] <- 'QUKE' # 816 is quercus ilicifolia which only grows in the eastern US so almost certainly a typo. this is a guess.
combinedtrees$`Species`[combinedtrees$`Species` == '839'] <- 'QUWI2'
combinedtrees$`Species`[combinedtrees$`Species` == '64'] <- 'JUOC'
combinedtrees$`Species`[combinedtrees$`Species` == '768'] <- 'PREM'
combinedtrees$`Species`[combinedtrees$`Species` == '21'] <- 'ABMAS' # Shasta Red Fir
combinedtrees$`Species`[combinedtrees$`Species` == '313'] <- 'ACNE2' # Box Elder
combinedtrees$`Species`[combinedtrees$`Species` == '492'] <- 'CONU4'
combinedtrees$`Species`[combinedtrees$`Species` == '999'] <- 'Unknown'
combinedtrees$`Species`[combinedtrees$`Species` == '9'] <- 'Unknown'
combinedtrees$`Species`[is.na(combinedtrees$`Species`)] <- "Unknown"
combinedtrees$`Species`[combinedtrees$`Species` == '231'] <- 'TABR2' #Pacific yew

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

combinedtrees$`Live/Dead`[combinedtrees$`Live/Dead` == '2'] <- 'Dead'

#### Correcting typos in the damage codes ####

# Some of the columns in the damage codes definitely belong to the snag decay class codes. These are dead trees with no snag decay class listed and a random 1-5 number listed in the damage code column. I am manually checking every single column before I change anything.

combinedtrees$`Snag Decay Class`[combinedtrees$`Live Tree Defects` == '5'] <- '5'

combinedtrees$`Live Tree Defects`[combinedtrees$`Live Tree Defects` == '5'] <- ''

combinedtrees$`Snag Decay Class`[combinedtrees$`Live Tree Defects` == '2'] <- '2'

combinedtrees$`Live Tree Defects`[combinedtrees$`Live Tree Defects` == '2'] <- ''

combinedtrees$`Snag Decay Class`[combinedtrees$`Live Tree Defects` == '1'] <- '1'

combinedtrees$`Live Tree Defects`[combinedtrees$`Live Tree Defects` == '1'] <- ''

# Weird but helpful for me: all the trees with a Live Tree Defect code of 3 already have that exact code in the adjacent canopy position column. These trees are all alive. I'm just deleting the code from the Live Tree Defect column.

combinedtrees$`Live Tree Defects`[combinedtrees$`Live Tree Defects` == '3'] <- ''

#### Convert canopy position into words ####

combinedtrees$`CanopyPosition`[combinedtrees$`CanopyPosition` == '1'] <- 'Open Grown'
combinedtrees$`CanopyPosition`[combinedtrees$`CanopyPosition` == '2'] <- 'Dominant'
combinedtrees$`CanopyPosition`[combinedtrees$`CanopyPosition` == '3'] <- 'Co-dominant'
combinedtrees$`CanopyPosition`[combinedtrees$`CanopyPosition` == '4'] <- 'Intermediate'
combinedtrees$`CanopyPosition`[combinedtrees$`CanopyPosition` == '5'] <- 'Overtopped'

combinedtrees$`CanopyPosition`[combinedtrees$`CanopyPosition` == '15'] <- ''

#### Combine with Batches 1-7 ####








#### Export ####

write.csv(combinedtrees, "C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\Batch8thru15trees.csv")





