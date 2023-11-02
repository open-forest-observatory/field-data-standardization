# Author: Emily Marie Purvis
# Date: 10.31.2023
# Goal: convert StemData_Batch4.xlsx individual tree coordinates from distance/azimuth to easting/northing and create stem map ggplots

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4")

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(pracma)
library(ggrepel)

#### Load and inspect tree data; small tweaks to get it standardized and ready for calculating tree coordinates ####

treedata = read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Plot_Data\\StemData_Batch4.xlsx"),sheet=1,col_names = TRUE)

# need to reformat the plot numbers in treedata so they match the plot data 

treedata$`Plot#`[treedata$`Plot#` == 'L-522'] <- 'L522'
treedata$`Plot#`[treedata$`Plot#` == 'L-528'] <- 'L528'
treedata$`Plot#`[treedata$`Plot#` == 'S-091'] <- 'S091'
treedata$`Plot#`[treedata$`Plot#` == 'S-907'] <- 'S907'
treedata$`Plot#`[treedata$`Plot#` == 'L-524'] <- 'L524'
treedata$`Plot#`[treedata$`Plot#` == 'L-520'] <- 'L520'
treedata$`Plot#`[treedata$`Plot#` == 'S-305'] <- 'S305'
treedata$`Plot#`[treedata$`Plot#` == 'L-535'] <- 'L535'
treedata$`Plot#`[treedata$`Plot#` == 'L-539'] <- 'L539'
treedata$`Plot#`[treedata$`Plot#` == 'L-534'] <- 'L534'

# need to delete random rows of all NAs in treedata

treedata <- treedata %>% drop_na(`Plot#`)

# R and I both get easily confused about the % slope column so I'm renaming it

treedata <- treedata %>% rename("PercentSlope" = `% slope`)

# some of the trees were measured with horizontal distance, some with slope + slope distance, some with both. we want all trees to have horizontal distance. 

# create a blank column for horizontal distance

treedata$HorizontalDistance <- 0

# if horizontal distance is NA, then calculate it from percent slope and slope distance, then put that value in the horizontal distance column. if horizontal distance is not NA, use the value the crew measured

for(i in 1:nrow(treedata)) {
  if(is.na(treedata$`H Dist`[i])) {
    treedata[i,]$HorizontalDistance = (treedata[i,]$`Slope Dist`)*cos(atan(treedata[i,]$PercentSlope/100))
  }
  else {
    treedata[i,]$HorizontalDistance = treedata[i,]$`H Dist`
  }
}

#### Load and inspect plot gps data ####

L522 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\L-522_g\\L-522.shp") 

L522 <- L522 %>%
  add_column(`Plot#` = "L522")

L522 <- L522 [-c(1:18, 21:22)]

# check CRS
# st_crs(L522)
# EPSG 4326

L528 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\L-528_g\\L-528.shp") 

L528 <- L528 %>%
  add_column(`Plot#` = "L528")

L528 <- L528 [-c(1:18, 21:22)]

# check CRS
# st_crs(L528)
# EPSG 4326

S091 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\S-091_g\\S-091.shp") 

S091 <- S091 %>%
  add_column(`Plot#` = "S091")

S091 <- S091 [-c(1:18, 21:22)]

# check CRS
# st_crs(S091)
# EPSG 4326

S907 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\S-907_g\\S-907.shp") 

S907 <- S907 %>%
  add_column(`Plot#` = "S907")

S907 <- S907 [-c(1:18, 21:22)]

# check CRS
# st_crs(S907)
# EPSG 4326

L524 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\L-524_g\\L-524.shp") 

L524 <- L524 %>%
  add_column(`Plot#` = "L524")

L524 <- L524 [-c(1:18, 21:22)]

# check CRS
# st_crs(L524)
# there is no crs, manually checking geospatial data and adding the correct crs
L524 <- L524 %>% st_set_crs(4326)

L520 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\L-520_g\\L-520.shp") 

L520 <- L520 %>%
  add_column(`Plot#` = "L520")

L520 <- L520 [-c(1:18, 21:22)]

# check CRS
# st_crs(L520)
# EPSG 4326

S305 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\S-305_g\\S-305.shp") 

S305 <- S305 %>%
  add_column(`Plot#` = "S305")

S305 <- S305 [-c(1:18, 21:22)]

# check CRS
# st_crs(S305)
# EPSG 4326

L535 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\L535_g\\L535.shp") 

L535 <- L535 %>%
  add_column(`Plot#` = "L535")

L535 <- L535 [-c(1:18, 21:22)]

# check CRS
# st_crs(L535)
# EPSG 4326

L539 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\L539_g\\L539.shp") 

L539 <- L539 %>%
  add_column(`Plot#` = "L539")

L539 <- L539 [-c(1:18, 21:22)]

# check CRS
# st_crs(L539)
# EPSG 4326

L534 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\L-534_g\\L-534.shp") 

L534 <- L534 %>%
  add_column(`Plot#` = "L534")

L534 <- L534 [-c(1:18, 21:22)]

# check CRS
# st_crs(L534)
# EPSG 4326

# combine all the plot data into one spatial dataframe

allplots <- rbind(L522, L528, S091, S907, L524, L520, S305, L535, L539, L534)

allplots <- allplots %>% rename (LatitudeWGS84 = Latitude, LongitudeWGS84 = Longitude)

allplots_UTM10N <- st_transform (allplots, 32610) 

# save the plot centers together in one spatial data file just in case it's useful later

st_write(allplots, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4_updatedplotcentersWGS84.gpkg"),delete_dsn=TRUE)

st_write(allplots_UTM10N, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4_updatedplotcentersUTM10N.gpkg"),delete_dsn=TRUE)

# extract the geometry features into columns of lat and long, then merge back into the spatial data frame

UTM10N_plotcoords <- data.frame(allplots_UTM10N$'Plot#', st_coordinates(allplots_UTM10N[,1], st_coordinates(allplots_UTM10N[,2]))) 

UTM10N_plotcoords <- UTM10N_plotcoords %>% rename ('Plot#'=allplots_UTM10N..Plot.., LongitudeUTM10N=X, LatitudeUTM10N=Y)

UTM10N_plotcoords <- UTM10N_plotcoords [-c(4:5)]

allplots_UTM10N <- full_join(allplots_UTM10N, UTM10N_plotcoords, by="Plot#")

#### Calculate the coordinates of each tree ####

# add new columns to the treedata dataframe so that each individual tree has a populated plot center easting and northing

trees_locations = full_join(treedata,st_drop_geometry(allplots_UTM10N),by='Plot#') %>% drop_na('HorizontalDistance')

trees_locations <- trees_locations %>% rename (PlotLongitudeUTM10N=LongitudeUTM10N, PlotLatitudeUTM10N=LatitudeUTM10N, PlotLongitudeWGS84=LongitudeWGS84, PlotLatitudeWGS84=LatitudeWGS84)

# calculate the coordinates of each tree using the plot center, horizontal distance, and azimuth 

trees_calcs = trees_locations %>%
  mutate(TreeLongitudeUTM10N = as.numeric(PlotLongitudeUTM10N) + sin(deg2rad(AZM)) * HorizontalDistance,
         TreeLatitudeUTM10N = as.numeric(PlotLatitudeUTM10N) + cos(deg2rad(AZM)) * HorizontalDistance)

# give trees ID numbers

trees_calcs = trees_calcs %>% mutate(tree_id = 1:nrow(trees_calcs))

#### convert to spatial data and export ####

trees_sp <- st_as_sf(trees_calcs, coords = c("TreeLongitudeUTM10N","TreeLatitudeUTM10N"), crs = 32610)

# exporting the tree spatial data in the same format as the plot data came in, 4326

st_write(trees_sp %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4_treecoordinatesWGS84.gpkg"),delete_dsn=TRUE)

st_write(trees_sp %>% st_transform(32610),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4_treecoordinatesUTM10N.gpkg"),delete_dsn=TRUE)

#### ggplots ####

# rename species so that the maps are more easily interpretable for normal people who don't have FIA codes memorized

trees_calcs$`Species`[trees_calcs$`Species` == '122'] <- 'PIPO'
trees_calcs$`Species`[trees_calcs$`Species` == '15'] <- 'ABCO'
trees_calcs$`Species`[trees_calcs$`Species` == '20'] <- 'ABMA'
trees_calcs$`Species`[trees_calcs$`Species` == '117'] <- 'PILA'
trees_calcs$`Species`[trees_calcs$`Species` == '101'] <- 'PIAL'
trees_calcs$`Species`[trees_calcs$`Species` == '119'] <- 'PIMO3'
trees_calcs$`Species`[trees_calcs$`Species` == '108'] <- 'PICOL'
trees_calcs$`Species`[trees_calcs$`Species` == '81'] <- 'CADE27'
trees_calcs$`Species`[trees_calcs$`Species` == '202'] <- 'PSME'
trees_calcs$`Species`[trees_calcs$`Species` == '127'] <- 'PISA2'
trees_calcs$`Species`[trees_calcs$`Species` == '116'] <- 'PIJE'
trees_calcs$`Species`[trees_calcs$`Species` == '103'] <- 'PIAT'
trees_calcs$`Species`[trees_calcs$`Species` == '361'] <- 'ARME'
trees_calcs$`Species`[trees_calcs$`Species` == '631'] <- 'LIDE3'
trees_calcs$`Species`[trees_calcs$`Species` == '312'] <- 'ACMA3'
trees_calcs$`Species`[trees_calcs$`Species` == '981'] <- 'UMCA'
trees_calcs$`Species`[trees_calcs$`Species` == '333'] <- 'AECA'
trees_calcs$`Species`[trees_calcs$`Species` == '805'] <- 'QUCH2'
trees_calcs$`Species`[trees_calcs$`Species` == '807'] <- 'QUDO'
trees_calcs$`Species`[trees_calcs$`Species` == '818'] <- 'QUKE'
trees_calcs$`Species`[trees_calcs$`Species` == '839'] <- 'QUWI2'
trees_calcs$`Species`[trees_calcs$`Species` == '64'] <- 'JUOC'
trees_calcs$`Species`[trees_calcs$`Species` == '768'] <- 'PREM'

# Renaming the latitude and longitude columns so they're shorter and more map-digestable

trees_calcs <- trees_calcs %>% rename (TreeLatitude=TreeLatitudeUTM10N, TreeLongitude=TreeLongitudeUTM10N) 

trees_calcs <- trees_calcs %>% rename (Latitude=TreeLatitude, Longitude=TreeLongitude) 

# rearranging the trees in descending DBH order will get the smaller points plotted on top for more easily interpretable maps

trees_calcs <- trees_calcs %>% arrange(desc(DBH))

# getting plot centers & trees in one data frame will make plotting easier

allplots_UTM10N <- st_drop_geometry(allplots_UTM10N) %>% rename (Longitude=PlotLongitudeUTM10N, Latitude=PlotLatitudeUTM10N)

all_trees <- as.data.frame(trees_calcs[,c('Plot#','Longitude','Latitude', 'Species', 'DBH', 'tree_id')])

allplots_UTM10N$Species <- 'Plot Center'

allplots_UTM10N$DBH <- 0

allplots_UTM10N$tree_id <- 'Plot Center'

all_plots <- as.data.frame(allplots_UTM10N [,c('Plot#','Longitude','Latitude', 'Species', 'DBH', 'tree_id')])

all_points <- rbind(all_plots, all_trees)

# plot L522

ggplot(subset(all_points,`Plot#`=="L522"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot L522") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(666450, 666675) + ylim (4371750, 4371960) -> plot_L522

# plot L528

ggplot(subset(all_points,`Plot#`=="L528"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot L528") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(667500, 667715) + ylim (4372470, 4372690) -> plot_L528

# plot S091

ggplot(subset(all_points,`Plot#`=="S091"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S091") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(691590, 691725) + ylim (4374045, 4374175) -> plot_S091

# plot S907

ggplot(subset(all_points,`Plot#`=="S907"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S907") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(687820, 687925) + ylim (4375595, 4375690) -> plot_S907

# plot L524

ggplot(subset(all_points,`Plot#`=="L524"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot L524") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(687995, 688200) + ylim (4375640, 4375850) -> plot_L524

# plot L520

ggplot(subset(all_points,`Plot#`=="L520"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot L520") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(687540, 687750) + ylim (4375480, 4375680) -> plot_L520

# plot S305

ggplot(subset(all_points,`Plot#`=="S305"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S305") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(688815, 688935) + ylim (4375620, 4375730) -> plot_S305

# plot L535

ggplot(subset(all_points,`Plot#`=="L535"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot L535") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(676145, 676355) + ylim (4373630, 4373835) -> plot_L535

# plot L539

ggplot(subset(all_points,`Plot#`=="L539"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot L539") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(673760, 673960) + ylim (4368470, 4368675) -> plot_L539

# plot L534

ggplot(subset(all_points,`Plot#`=="L534"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot L534") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(669715, 669915) + ylim (4370065, 4370260) -> plot_L534

# export plots to pdf

pdf(file="C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\StemMap4_ggplots.pdf",height = 8, width = 8)
plot(plot_L522)
plot(plot_L528)
plot(plot_S091)
plot(plot_S907)
plot(plot_L524)
plot(plot_L520)
plot(plot_S305)
plot(plot_L535)
plot(plot_L539)
plot(plot_L534)
dev.off()


