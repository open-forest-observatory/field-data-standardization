# Author: Emily Marie Purvis
# Date: 11.1.2023
# Goal: convert StemData_Batch6.xlsx individual tree coordinates from distance/azimuth to easting/northing and create stem map ggplots

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6")

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(pracma)
library(ggrepel)

#### Load and inspect tree data; small tweaks to get it standardized and ready for calculating tree coordinates ####

treedata = read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Plot_Data\\StemData_Batch6.xlsx"),sheet=1,col_names = TRUE)

# need to reformat the plot numbers in treedata so they match the plot data 

treedata <- treedata %>% rename(`Plot#` = `Plot #`)

treedata$`Plot#`[treedata$`Plot#` == 'S-119'] <- 'S119'
treedata$`Plot#`[treedata$`Plot#` == 'S-965'] <- 'S965'
treedata$`Plot#`[treedata$`Plot#` == 'S-104'] <- 'S104'
treedata$`Plot#`[treedata$`Plot#` == 'S-110'] <- 'S110'
treedata$`Plot#`[treedata$`Plot#` == 'S-112'] <- 'S112'
treedata$`Plot#`[treedata$`Plot#` == 'S-114'] <- 'S114'
treedata$`Plot#`[treedata$`Plot#` == 'S-115'] <- 'S115'
treedata$`Plot#`[treedata$`Plot#` == 'S-117'] <- 'S117'
treedata$`Plot#`[treedata$`Plot#` == 'S-120'] <- 'S120'
treedata$`Plot#`[treedata$`Plot#` == 'S-503'] <- 'S503'

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

S119 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S119\\S119.shp") 

S119 <- S119 %>%
  add_column(`Plot#` = "S119")

S119 <- S119 [-c(1:18, 21:22)]

# check CRS
# st_crs(S119)
# EPSG 4326

S965 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S965\\S965.shp") 

S965 <- S965 %>%
  add_column(`Plot#` = "S965")

S965 <- S965 [-c(1:19, 22:23)]

# check CRS
# st_crs(S965)
# EPSG 4326

S104 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S-104\\S-104.shp") 

S104 <- S104 %>%
  add_column(`Plot#` = "S104")

S104 <- S104 [-c(1:19, 22:23)]

# check CRS
# st_crs(S104)
# EPSG 4326

S110 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S-110\\S-110.shp") 

S110 <- S110 %>%
  add_column(`Plot#` = "S110")

S110 <- S110 [-c(1:19, 22:23)]

# check CRS
# st_crs(S110)
# EPSG 4326

S112 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S112\\S112.shp") 

S112 <- S112 %>%
  add_column(`Plot#` = "S112")

S112 <- S112 [-c(1:18, 21:22)]

# check CRS
# st_crs(S112)
# EPSG 4326

S114 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S114\\S114.shp") 

S114 <- S114 %>%
  add_column(`Plot#` = "S114")

S114 <- S114 [-c(1:18, 21:22)]

# check CRS
# st_crs(S114)
# EPSG 4326

S115 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S115\\S115.shp") 

S115 <- S115 %>%
  add_column(`Plot#` = "S115")

S115 <- S115 [-c(1:18, 21:22)]

# check CRS
# st_crs(S115)
# EPSG 4326

S117 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S117\\S117.shp") 

S117 <- S117 %>%
  add_column(`Plot#` = "S117")

S117 <- S117 [-c(1:18, 21:22)]

# check CRS
# st_crs(S117)
# EPSG 4326

S120 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S120\\S120.shp") 

S120 <- S120 %>%
  add_column(`Plot#` = "S120")

S120 <- S120 [-c(1:18, 21:22)]

# check CRS
# st_crs(S120)
# EPSG 4326

S503 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S503\\S503.shp") 

S503 <- S503 %>%
  add_column(`Plot#` = "S503")

S503 <- S503 [-c(1:18, 21:22)]

# check CRS
# st_crs(S503)
# EPSG 4326

# combine all the plot data into one spatial dataframe

allplots <- rbind(S119, S965, S104, S110, S112, S114, S115, S117, S120, S503)

allplots <- allplots %>% rename (PlotLatitudeWGS84 = Latitude, PlotLongitudeWGS84 = Longitude)

allplots_UTM10N <- st_transform (allplots, 32610) 

# save the plot centers together in one spatial data file just in case it's useful later

st_write(allplots, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Stem_Batch_6_updatedplotcentersWGS84.gpkg"),delete_dsn=TRUE)

st_write(allplots_UTM10N, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Stem_Batch_6_updatedplotcentersUTM10N.gpkg"),delete_dsn=TRUE)

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

st_write(trees_sp %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Stem_Batch_6_treecoordinatesWGS84.gpkg"),delete_dsn=TRUE)

st_write(trees_sp %>% st_transform(32610),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Stem_Batch_6_treecoordinatesUTM10N.gpkg"),delete_dsn=TRUE)

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

# plot S119

ggplot(subset(all_points,`Plot#`=="S119"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S119") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(714760, 714870) + ylim (4375730, 4375840) -> plot_S119

# plot S965

ggplot(subset(all_points,`Plot#`=="S965"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S965") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(715830, 715940) + ylim (4376500, 4376630) -> plot_S965

# plot S104

ggplot(subset(all_points,`Plot#`=="S104"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S104") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(698530, 698645) + ylim (4389110, 4389230) -> plot_S104

# plot S110

ggplot(subset(all_points,`Plot#`=="S110"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S110") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(704985, 705075) + ylim (4391150, 4391275) -> plot_S110

# plot S112

ggplot(subset(all_points,`Plot#`=="S112"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S112") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(707165, 707300) + ylim (4380110, 4380240) -> plot_S112

# plot S114

ggplot(subset(all_points,`Plot#`=="S114"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S114") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(710955, 711075) + ylim (4378370, 4378490) -> plot_S114

# plot S115

ggplot(subset(all_points,`Plot#`=="S115"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S115") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(710890, 710990) + ylim (4381185, 4381295) -> plot_S115

# plot S117

ggplot(subset(all_points,`Plot#`=="S117"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S117") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(711455, 711570) + ylim (4391390, 4391515) -> plot_S117

# plot S120

ggplot(subset(all_points,`Plot#`=="S120"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S120") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(715565, 715695) + ylim (4379505, 4379590) -> plot_S120

# plot S503

ggplot(subset(all_points,`Plot#`=="S503"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S503") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(704905, 705020) + ylim (4388150, 4388270) -> plot_S503

# export plots to pdf

pdf(file="C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\StemMap6_ggplots.pdf",height = 8, width = 8)
plot(plot_S119)
plot(plot_S965)
plot(plot_S104)
plot(plot_S110)
plot(plot_S112)
plot(plot_S114)
plot(plot_S115)
plot(plot_S117)
plot(plot_S120)
plot(plot_S503)
dev.off()


