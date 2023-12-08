# Author: Emily Marie Purvis
# Date: 12.6.2023
# Goal: convert Batch 15 individual tree coordinates from distance/azimuth to easting/northing and create stem map ggplots

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_15\\Batch_15")

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(pracma)
library(ggrepel)

#### Load and inspect tree data; small tweaks to get it standardized and ready for calculating tree coordinates ####

treedata = read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_15\\Batch15\\Plot_Data\\Stem_Batch15.xlsx"),sheet=1,col_names = TRUE)

# need to reformat the plot numbers in treedata so they match the plot data 

treedata$`Plot #`[treedata$`Plot #` == 'S-005'] <- 'S005'
treedata$`Plot #`[treedata$`Plot #` == 'S-018'] <- 'S018'
treedata$`Plot #`[treedata$`Plot #` == 'S-020'] <- 'S020'
treedata$`Plot #`[treedata$`Plot #` == 'S-024'] <- 'S024'
treedata$`Plot #`[treedata$`Plot #` == 'S-041'] <- 'S041'
treedata$`Plot #`[treedata$`Plot #` == 'S-309'] <- 'S309'
treedata$`Plot #`[treedata$`Plot #` == 'S-501'] <- 'S501'

# need to delete random rows of all NAs in treedata

treedata <- treedata %>% drop_na(`Plot #`)

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

plotcenters <- read_sf ("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_15\\Stem_Batch_15_plotcoordinatesWGS84.gpkg")

plotcenters <- plotcenters %>% rename(`Plot #` = `Name`)

plotcenters_UTM10N <- st_transform (plotcenters, 32610) 

# extract the geometry features into columns of lat and long, then merge back into the spatial data frame

UTM10N_plotcoords <- data.frame(plotcenters_UTM10N$'Plot #', st_coordinates(plotcenters_UTM10N[,1], st_coordinates(plotcenters_UTM10N[,2]))) 

UTM10N_plotcoords <- UTM10N_plotcoords %>% rename ('Plot #'=plotcenters_UTM10N..Plot..., PlotLongitudeUTM10N=X, PlotLatitudeUTM10N=Y)

UTM10N_plotcoords <- UTM10N_plotcoords [-c(4:5)]

plotcenters_UTM10N <- full_join(plotcenters_UTM10N, UTM10N_plotcoords, by="Plot #")

#### Calculate the coordinates of each tree ####

# add new columns to the treedata dataframe so that each individual tree has a populated plot center easting and northing

treedata = full_join(treedata,st_drop_geometry(plotcenters_UTM10N),by='Plot #')

# calculate the coordinates of each tree using the plot center, horizontal distance, and azimuth 

treedata = treedata %>%
  mutate(TreeLongitudeUTM10N = as.numeric(PlotLongitudeUTM10N) + sin(deg2rad(AZM)) * HorizontalDistance,
         TreeLatitudeUTM10N = as.numeric(PlotLatitudeUTM10N) + cos(deg2rad(AZM)) * HorizontalDistance)

# give trees ID numbers

treedata = treedata %>% mutate(tree_id = 1:nrow(treedata))

#### convert to spatial data and export ####

trees_sp <- st_as_sf(treedata, coords = c("TreeLongitudeUTM10N","TreeLatitudeUTM10N"), crs = 32610)

# exporting the tree spatial data in the same format as the plot data came in, 4326

st_write(trees_sp %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_15\\Stem_Batch_15_treecoordinatesWGS84.gpkg"),delete_dsn=TRUE)

st_write(trees_sp %>% st_transform(32610),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_15\\Stem_Batch_15_treecoordinatesUTM10N.gpkg"),delete_dsn=TRUE)

#### ggplots ####

# rename species so that the maps are more easily interpretable for normal people who don't have FIA codes memorized

treedata$`Species`[treedata$`Species` == '122'] <- 'PIPO'
treedata$`Species`[treedata$`Species` == '15'] <- 'ABCO'
treedata$`Species`[treedata$`Species` == '20'] <- 'ABMA'
treedata$`Species`[treedata$`Species` == '117'] <- 'PILA'
treedata$`Species`[treedata$`Species` == '101'] <- 'PIAL'
treedata$`Species`[treedata$`Species` == '119'] <- 'PIMO3'
treedata$`Species`[treedata$`Species` == '108'] <- 'PICOL'
treedata$`Species`[treedata$`Species` == '81'] <- 'CADE27'
treedata$`Species`[treedata$`Species` == '202'] <- 'PSME'
treedata$`Species`[treedata$`Species` == '127'] <- 'PISA2'
treedata$`Species`[treedata$`Species` == '116'] <- 'PIJE'
treedata$`Species`[treedata$`Species` == '103'] <- 'PIAT'
treedata$`Species`[treedata$`Species` == '361'] <- 'ARME'
treedata$`Species`[treedata$`Species` == '631'] <- 'LIDE3'
treedata$`Species`[treedata$`Species` == '312'] <- 'ACMA3'
treedata$`Species`[treedata$`Species` == '981'] <- 'UMCA'
treedata$`Species`[treedata$`Species` == '333'] <- 'AECA'
treedata$`Species`[treedata$`Species` == '805'] <- 'QUCH2'
treedata$`Species`[treedata$`Species` == '807'] <- 'QUDO'
treedata$`Species`[treedata$`Species` == '818'] <- 'QUKE'
treedata$`Species`[treedata$`Species` == '839'] <- 'QUWI2'
treedata$`Species`[treedata$`Species` == '64'] <- 'JUOC'
treedata$`Species`[treedata$`Species` == '768'] <- 'PREM'
treedata$`Species`[treedata$`Species` == '816'] <- 'QUKE' # 816 is quercus ilicifolia which only grows in the eastern US so almost certainly a typo. this is a guess.
treedata$`Species`[treedata$`Species` == '212'] <- 'PIPO' # 212 is giant sequoia, almost certainly a typo
treedata$`Species`[treedata$`Species` == '188'] <- 'QUKE'# another typo I'm guessing at the right answer for-- 188 isn't a code for anything

# Renaming the latitude and longitude columns so the tree and plot data match-- this will allow me to plot both on the same axes

treedata <- treedata %>% rename (Latitude=TreeLatitudeUTM10N, Longitude=TreeLongitudeUTM10N)  

# rearranging the trees in descending DBH order will get the smaller points plotted on top for more easily interpretable maps

treedata <- treedata %>% arrange(desc(DBH))

# getting plot centers & trees in one data frame will make plotting easier

plotcenters_UTM10N <- st_drop_geometry(plotcenters_UTM10N) %>% rename (Longitude=PlotLongitudeUTM10N, Latitude=PlotLatitudeUTM10N)

all_trees <- as.data.frame(treedata[,c('Plot #','Longitude','Latitude', 'Species', 'DBH', 'tree_id')])

plotcenters_UTM10N$Species <- 'Plot Center'

plotcenters_UTM10N$DBH <- 0

plotcenters_UTM10N$tree_id <- 'Plot Center'

all_plots <- as.data.frame(plotcenters_UTM10N [,c('Plot #','Longitude','Latitude', 'Species', 'DBH', 'tree_id')])

all_points <- rbind(all_plots, all_trees)

# plot S005

ggplot(subset(all_points,`Plot #`=="S005"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S005") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(663105, 663210) + ylim (4365710, 4365825) -> plot_S005

# plot S018

ggplot(subset(all_points,`Plot #`=="S018"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S018") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(670350, 670470) + ylim (4370590, 4370700) -> plot_S018

# plot S020

ggplot(subset(all_points,`Plot #`=="S020"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S020") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(671235, 671350) + ylim (4372450, 4372580) -> plot_S020

# plot S024

ggplot(subset(all_points,`Plot #`=="S024"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S024") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(672225, 672340) + ylim (4370600, 4370735) -> plot_S024

# plot S041

ggplot(subset(all_points,`Plot #`=="S041"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S041") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(675590, 675705) + ylim (4380025, 4380130) -> plot_S041

# plot S309

ggplot(subset(all_points,`Plot #`=="S309"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S309") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(670380, 670510) + ylim (4372510, 4372635) -> plot_S309

# plot S501

ggplot(subset(all_points,`Plot #`=="S501"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S501") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(674025, 674135) + ylim (4368635, 4368755) -> plot_S501

# export plots to pdf

pdf(file="C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_15\\StemMap15_ggplots.pdf",height = 8, width = 8)
plot(plot_S005)
plot(plot_S018)
plot(plot_S020)
plot(plot_S024)
plot(plot_S041)
plot(plot_S309)
plot(plot_S501)
dev.off()
