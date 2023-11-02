# Author: Emily Marie Purvis
# Date: 11.1.2023
# Goal: convert StemData_Batch7.xlsx individual tree coordinates from distance/azimuth to easting/northing and create stem map ggplots

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7")

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(pracma)
library(ggrepel)

#### Load and inspect tree data; small tweaks to get it standardized and ready for calculating tree coordinates ####

treedata = read_excel(data("Plot_Data\\Stem_Batch7.xlsx"),sheet=1,col_names = TRUE)

# need to reformat the plot numbers in treedata so they match the plot data 

treedata <- treedata %>% rename(`Plot#` = `Plot #`)

treedata$`Plot#`[treedata$`Plot#` == 'S-303'] <- 'S303'
treedata$`Plot#`[treedata$`Plot#` == 'S-931'] <- 'S931'
treedata$`Plot#`[treedata$`Plot#` == 'S-037'] <- 'S037'
treedata$`Plot#`[treedata$`Plot#` == 'S-058'] <- 'S058'
treedata$`Plot#`[treedata$`Plot#` == 'S-065'] <- 'S065'
treedata$`Plot#`[treedata$`Plot#` == 'S-066'] <- 'S066'
treedata$`Plot#`[treedata$`Plot#` == 'S-070'] <- 'S070'
treedata$`Plot#`[treedata$`Plot#` == 'S-076'] <- 'S076'
treedata$`Plot#`[treedata$`Plot#` == 'S-989'] <- 'S989'
treedata$`Plot#`[treedata$`Plot#` == 'S-071'] <- 'S071'

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

#### Load and inspect plot gps data ####

S303 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S-303\\S-303.shp") 

S303 <- S303 %>%
  add_column(`Plot#` = "S303")

S303 <- S303 [-c(1:19, 22:23)]

# check CRS
# st_crs(S303)
# EPSG 4326

S931 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S-931\\S-931.shp") 

S931 <- S931 %>%
  add_column(`Plot#` = "S931")

S931 <- S931 [-c(1:19, 22:23)]

# check CRS
# st_crs(S931)
# EPSG 4326

S037 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S037\\S037.shp") 

S037 <- S037 %>%
  add_column(`Plot#` = "S037")

S037 <- S037 [-c(1:19, 22:23)]

# check CRS
# st_crs(S037)
# EPSG 4326

S058 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S058\\S058.shp") 

S058 <- S058 %>%
  add_column(`Plot#` = "S058")

S058 <- S058 [-c(1:19, 22:23)]

# check CRS
# st_crs(S058)
# EPSG 4326

S065 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S065\\S065.shp") 

S065 <- S065 %>%
  add_column(`Plot#` = "S065")

S065 <- S065 [-c(1:19, 22:23)]

# check CRS
# st_crs(S065)
# EPSG 4326

S066 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S066\\S066.shp") 

S066 <- S066 %>%
  add_column(`Plot#` = "S066")

S066 <- S066 [-c(1:19, 22:23)]

# check CRS
# st_crs(S066)
# EPSG 4326

S070 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S070\\S070.shp") 

S070 <- S070 %>%
  add_column(`Plot#` = "S070")

S070 <- S070 [-c(1:19, 22:23)]

# check CRS
# st_crs(S070)
# EPSG 4326

S076 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S076\\S076.shp") 

S076 <- S076 %>%
  add_column(`Plot#` = "S076")

S076 <- S076 [-c(1:19, 22:23)]

# check CRS
# st_crs(S076)
# EPSG 4326

S989 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S989\\S989.shp") 

S989 <- S989 %>%
  add_column(`Plot#` = "S989")

S989 <- S989 [-c(1:19, 22:23)]

# check CRS
# st_crs(S989)
# EPSG 4326

S071 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S-071\\S-071.shp") 

S071 <- S071 %>%
  add_column(`Plot#` = "S071")

S071 <- S071 [-c(1:19, 22:23)]

# check CRS
# st_crs(S071)
# EPSG 4326

# combine all the plot data into one spatial dataframe

allplots <- rbind(S303, S931, S037, S058, S065, S066, S070, S076, S989, S071)

allplots <- allplots %>% rename (PlotLatitudeWGS84 = Latitude, PlotLongitudeWGS84 = Longitude)

allplots_UTM10N <- st_transform (allplots, 32610) 

# save the plot centers together in one spatial data file just in case it's useful later

st_write(allplots, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Stem_Batch_7_updatedplotcentersWGS84.gpkg"),delete_dsn=TRUE)

st_write(allplots_UTM10N, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Stem_Batch_7_updatedplotcentersUTM10N.gpkg"),delete_dsn=TRUE)

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
  mutate(TreeLongitudeUTM10N = as.numeric(PlotLongitudeUTM10N) + sin(deg2rad(AZM)) * HorizontalDistance,
         TreeLatitudeUTM10N = as.numeric(PlotLatitudeUTM10N) + cos(deg2rad(AZM)) * HorizontalDistance)

# give trees ID numbers

trees_calcs = trees_calcs %>% mutate(tree_id = 1:nrow(trees_calcs))

#### convert to spatial data and export ####

trees_sp <- st_as_sf(trees_calcs, coords = c("TreeLongitudeUTM10N","TreeLatitudeUTM10N"), crs = 32610)

# exporting the tree spatial data in the same format as the plot data came in, 4326

st_write(trees_sp %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Stem_Batch_7_treecoordinatesWGS84.gpkg"),delete_dsn=TRUE)

st_write(trees_sp %>% st_transform(32610),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Stem_Batch_7_treecoordinatesUTM10N.gpkg"),delete_dsn=TRUE)

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

# Renaming the latitude and longitude columns so the tree and plot data match-- this will allow me to plot both on the same axes

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

# plot S303

ggplot(subset(all_points,`Plot#`=="S303"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S303") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(707450, 707570) + ylim (4387730, 4387870) -> plot_S303

# plot S931

ggplot(subset(all_points,`Plot#`=="S931"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S931") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(704320, 704485) + ylim (4391120, 4391260) -> plot_S931

# plot S037

ggplot(subset(all_points,`Plot#`=="S037"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S037") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(674705, 674820) + ylim (4379985, 4380115) -> plot_S037

# plot S058

ggplot(subset(all_points,`Plot#`=="S058"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S058") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(680060, 680170) + ylim (4386670, 4386760) -> plot_S058

# plot S065

ggplot(subset(all_points,`Plot#`=="S065"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S065") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(680960, 681055) + ylim (4387580, 4387690) -> plot_S065

# plot S066

ggplot(subset(all_points,`Plot#`=="S066"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S066") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(680840, 680970) + ylim (4390410, 4390525) -> plot_S066

# plot S070

ggplot(subset(all_points,`Plot#`=="S070"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S070") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(681880, 681995) + ylim (4387640, 4387740) -> plot_S070

# plot S076

ggplot(subset(all_points,`Plot#`=="S076"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S076") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(683700, 683820) + ylim (4388615, 4388730) -> plot_S076

# plot S989

ggplot(subset(all_points,`Plot#`=="S989"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S989") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(682785, 682900) + ylim (4391420, 4391530) -> plot_S989

# plot S071

ggplot(subset(all_points,`Plot#`=="S071"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S071") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(681740, 681825) + ylim (4391330, 4391440) -> plot_S071

# export plots to pdf

pdf(file="C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\StemMap7_ggplots.pdf",height = 8, width = 8)
plot(plot_S303)
plot(plot_S931)
plot(plot_S037)
plot(plot_S058)
plot(plot_S065)
plot(plot_S066)
plot(plot_S070)
plot(plot_S076)
plot(plot_S989)
plot(plot_S071)
dev.off()


