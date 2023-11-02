# Author: Emily Marie Purvis
# Date: 10.31.2023
# Goal: convert StemData_Batch5.xlsx individual tree coordinates from distance/azimuth to easting/northing and create stem map ggplots

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5")

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(pracma)
library(ggrepel)

#### Load and inspect tree data; small tweaks to get it standardized and ready for calculating tree coordinates ####

treedata = read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Plot_Data\\Stem_Batch5.xlsx"),sheet=1,col_names = TRUE)

# need to reformat the plot numbers in treedata so they match the plot data 

treedata <- treedata %>% rename(`Plot#` = `Plot #`)

treedata$`Plot#`[treedata$`Plot#` == 'L-092'] <- 'L092'
treedata$`Plot#`[treedata$`Plot#` == 'S-100'] <- 'S100'
treedata$`Plot#`[treedata$`Plot#` == 'S-103'] <- 'S103'
treedata$`Plot#`[treedata$`Plot#` == 'S-099'] <- 'S099'
treedata$`Plot#`[treedata$`Plot#` == 'S-105'] <- 'S105'
treedata$`Plot#`[treedata$`Plot#` == 'S-111'] <- 'S111'
treedata$`Plot#`[treedata$`Plot#` == 'S-098'] <- 'S098'
treedata$`Plot#`[treedata$`Plot#` == 'S-101'] <- 'S101'
treedata$`Plot#`[treedata$`Plot#` == 'S-108'] <- 'S108'
treedata$`Plot#`[treedata$`Plot#` == 'S-106'] <- 'S106'

# need to delete random rows of all NAs in treedata

treedata <- treedata %>% drop_na(`Plot#`)

# R and I both get easily confused about the % slope column so I'm renaming it

treedata <- treedata %>% rename("PercentSlope" = `% slope`)

#### Load and inspect plot gps data ####

L092 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\L-092\\S092.shp") 

L092 <- L092 %>%
 add_column(`Plot#` = "L092")

 L092 <- L092 [-c(1:18, 21:22)]

# check CRS
# st_crs(L092)
# EPSG 4326

S100 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\S-100\\S-100.shp") 

S100 <- S100 %>%
  add_column(`Plot#` = "S100")

S100 <- S100 [-c(1:19, 22:23)]

# check CRS
# st_crs(S100)
# EPSG 4326

S103 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\S-103\\S-103.shp")  

S103 <- S103 %>%
  add_column(`Plot#` = "S103")

S103 <- S103 [-c(1:19, 22:23)]

# check CRS
# st_crs(S103)
# EPSG 4326

S099 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\S099\\S099.shp")  

S099 <- S099 %>%
  add_column(`Plot#` = "S099")

S099 <- S099 [-c(1:18, 21:22)]

# check CRS
# st_crs(S099)
# EPSG 4326

S105 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\S105\\S105.shp") 

S105 <- S105 %>%
  add_column(`Plot#` = "S105")

S105 <- S105 [-c(1:18, 21:22)]

# check CRS
# st_crs(S105)
# EPSG 4326

S111 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\S111\\S111.shp") 

S111 <- S111 %>%
  add_column(`Plot#` = "S111")

S111 <- S111 [-c(1:18, 21:22)]

# check CRS
# st_crs(S111)
# EPSG 4326

S098 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\S098\\S098.shp") 

S098 <- S098 %>%
  add_column(`Plot#` = "S098")

S098 <- S098 [-c(1:18, 21:22)]

# check CRS
# st_crs(S098)
# EPSG 4326

S101 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\S101\\S101.shp") 

S101 <- S101 %>%
  add_column(`Plot#` = "S101")

S101 <- S101 [-c(1:18, 21:22)]

# check CRS
# st_crs(S101)
# EPSG 4326

S108 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\S108\\S108.shp") 

S108 <- S108 %>%
  add_column(`Plot#` = "S108")

S108 <- S108 [-c(1:18, 21:22)]

# check CRS
# st_crs(S108)
# EPSG 4326

S106 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\S106\\S106.shp") 

S106 <- S106 %>%
  add_column(`Plot#` = "S106")

S106 <- S106 [-c(1:18, 21:22)]

# check CRS
# st_crs(S106)
# EPSG 4326

# combine all the plot data into one spatial dataframe

allplots <- rbind(L092, S100, S103, S099, S105, S111, S098, S101, S108, S106)

allplots <- allplots %>% rename (PlotLatitudeWGS84 = Latitude, PlotLongitudeWGS84 = Longitude)

allplots_UTM10N <- st_transform (allplots, 32610) 

# save the plot centers together in one spatial data file just in case it's useful later

st_write(allplots, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Stem_Batch_5_updatedplotcentersWGS84.gpkg"),delete_dsn=TRUE)

st_write(allplots_UTM10N, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Stem_Batch_5_updatedplotcentersUTM10N.gpkg"),delete_dsn=TRUE)

# extract the geometry features into columns of lat and long, then merge back into the spatial data frame

UTM10N_plotcoords <- data.frame(allplots_UTM10N$'Plot#', st_coordinates(allplots_UTM10N[,1], st_coordinates(allplots_UTM10N[,2]))) 

UTM10N_plotcoords <- UTM10N_plotcoords %>% rename ('Plot#'=allplots_UTM10N..Plot.., PlotLongitudeUTM10N=X, PlotLatitudeUTM10N=Y)

UTM10N_plotcoords <- UTM10N_plotcoords [-c(4:5)]

allplots_UTM10N <- full_join(allplots_UTM10N, UTM10N_plotcoords, by="Plot#")

#### Calculate the coordinates of each tree ####

# add new columns to the treedata dataframe so that each individual tree has a populated plot center easting and northing

trees_locations = full_join(treedata,st_drop_geometry(allplots_UTM10N),by='Plot#')

# calculate the coordinates of each tree using the plot center, horizontal distance, and azimuth 

trees_calcs = trees_locations %>%
  mutate(TreeLongitudeUTM10N = as.numeric(PlotLongitudeUTM10N) + sin(deg2rad(AZM)) * `H Distance`,
         TreeLatitudeUTM10N = as.numeric(PlotLatitudeUTM10N) + cos(deg2rad(AZM)) * `H Distance`)

# give trees ID numbers

trees_calcs = trees_calcs %>% mutate(tree_id = 1:nrow(trees_calcs))

#### convert to spatial data and export ####

trees_sp <- st_as_sf(trees_calcs, coords = c("TreeLongitudeUTM10N","TreeLatitudeUTM10N"), crs = 32610)

# exporting the tree spatial data in the same format as the plot data came in, 4326

st_write(trees_sp %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Stem_Batch_5_treecoordinatesWGS84.gpkg"),delete_dsn=TRUE)

st_write(trees_sp %>% st_transform(32610),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Stem_Batch_5_treecoordinatesUTM10N.gpkg"),delete_dsn=TRUE)

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
trees_calcs$`Species`[trees_calcs$`Species` == '492'] <- 'CONU4'

# Renaming the latitude and longitude columns so they're shorter and more map-digestable

trees_calcs <- trees_calcs %>% rename (Latitude=TreeLatitudeUTM10N, Longitude=TreeLongitudeUTM10N) 

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

# plot L092

ggplot(subset(all_points,`Plot#`=="L092"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot L092") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(662135, 662360) + ylim (4365275, 4365490) -> plot_L092

# plot S100

ggplot(subset(all_points,`Plot#`=="S100"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S100") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(696800, 696920) + ylim (4385350, 4385465) -> plot_S100

# plot S103

ggplot(subset(all_points,`Plot#`=="S103"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S103") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(698600, 698710) + ylim (4387265, 4387370) -> plot_S103

# plot S099

ggplot(subset(all_points,`Plot#`=="S099"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S099") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(695805, 695925) + ylim (4387160, 4387275) -> plot_S099

# plot S105

ggplot(subset(all_points,`Plot#`=="S105"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S105") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(698485, 698610) + ylim (4390955, 4391080) -> plot_S105

# plot S111

ggplot(subset(all_points,`Plot#`=="S111"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S111") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(706230, 706340) + ylim (4381015, 4381135) -> plot_S111

# plot S098

ggplot(subset(all_points,`Plot#`=="S098"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S098") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(694860, 694970) + ylim (4387130, 4387260) -> plot_S098

# plot S101

ggplot(subset(all_points,`Plot#`=="S101"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S101") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(696765, 696890) + ylim (4386265, 4386385) -> plot_S101

# plot S108

ggplot(subset(all_points,`Plot#`=="S108"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S108") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(704105, 704220) + ylim (4390215, 4390340) -> plot_S108

# plot S106

ggplot(subset(all_points,`Plot#`=="S106"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S106") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(698480, 698530) + ylim (4391875, 4391965) -> plot_S106

# export plots to pdf

pdf(file="C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\StemMap5_ggplots.pdf",height = 8, width = 8)
plot(plot_L092)
plot(plot_S100)
plot(plot_S103)
plot(plot_S099)
plot(plot_S105)
plot(plot_S111)
plot(plot_S098)
plot(plot_S101)
plot(plot_S108)
plot(plot_S106)
dev.off()

