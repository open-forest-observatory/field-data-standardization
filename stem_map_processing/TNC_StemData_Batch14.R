# Author: Emily Marie Purvis
# Date: 12.6.2023
# Goal: convert Batch 14 individual tree coordinates from distance/azimuth to easting/northing and create stem map ggplots

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_14\\Batch_14")

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(pracma)
library(ggrepel)

#### Load and inspect tree data; small tweaks to get it standardized and ready for calculating tree coordinates ####

treedata = read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_14\\Batch14\\Plot_Data\\Stem_Batch14_emp_edits.xlsx"),sheet=1,col_names = TRUE)

# need to reformat the plot numbers in treedata so they match the plot data 

treedata$`Plot #`[treedata$`Plot #` == 'S-049'] <- 'S049'
treedata$`Plot #`[treedata$`Plot #` == 'S-004'] <- 'S004'
treedata$`Plot #`[treedata$`Plot #` == 'S-010'] <- 'S010'
treedata$`Plot #`[treedata$`Plot #` == 'S-011'] <- 'S011'
treedata$`Plot #`[treedata$`Plot #` == 'S-013'] <- 'S013'
treedata$`Plot #`[treedata$`Plot #` == 'S-017'] <- 'S017'
treedata$`Plot #`[treedata$`Plot #` == 'S-043'] <- 'S043'
treedata$`Plot #`[treedata$`Plot #` == 'S-086'] <- 'S086'
treedata$`Plot #`[treedata$`Plot #` == 'S-311'] <- 'S311'
treedata$`Plot #`[treedata$`Plot #` == 'S-506'] <- 'S506'

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

plotcenters <- read_sf ("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_14\\Stem_Batch_14_plotcoordinatesWGS84.gpkg")

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

st_write(trees_sp %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_14\\Stem_Batch_14_treecoordinatesWGS84.gpkg"),delete_dsn=TRUE)

st_write(trees_sp %>% st_transform(32610),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_14\\Stem_Batch_14_treecoordinatesUTM10N.gpkg"),delete_dsn=TRUE)

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

# plot S049

ggplot(subset(all_points,`Plot #`=="S049"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S049") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(678385, 678495) + ylim (4381030, 4381155) -> plot_S049

# plot S004

ggplot(subset(all_points,`Plot #`=="S004"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S004") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(663110, 663220) + ylim (4364760, 4364875) -> plot_S004

# plot S010

ggplot(subset(all_points,`Plot #`=="S010"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S010") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(664885, 664980) + ylim (4366710, 4366820) -> plot_S010

# plot S011

ggplot(subset(all_points,`Plot #`=="S011"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S011") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(664745, 664850) + ylim (4372250, 4372370) -> plot_S011

# plot S013

ggplot(subset(all_points,`Plot #`=="S013"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S013") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(666685, 666810) + ylim (4369540, 4369635) -> plot_S013

# plot S017

ggplot(subset(all_points,`Plot #`=="S017"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S017") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(670400, 670515) + ylim (4369665, 4369765) -> plot_S017

# plot S043

ggplot(subset(all_points,`Plot #`=="S043"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S043") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(676515, 676570) + ylim (4381920, 4381980) -> plot_S043

# plot S086

ggplot(subset(all_points,`Plot #`=="S086"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S086") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(686925, 687025) + ylim (4375730, 4375835) -> plot_S086

# plot S311

ggplot(subset(all_points,`Plot #`=="S311"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S311") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(673715, 673810) + ylim (4378625, 4378735) -> plot_S311

# plot S506

ggplot(subset(all_points,`Plot #`=="S506"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S506") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(681895, 682010) + ylim (4372905, 4373000) -> plot_S506

# export plots to pdf

pdf(file="C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_14\\StemMap14_ggplots.pdf",height = 8, width = 8)
plot(plot_S004)
plot(plot_S049)
plot(plot_S010)
plot(plot_S011)
plot(plot_S013)
plot(plot_S017)
plot(plot_S043)
plot(plot_S086)
plot(plot_S311)
plot(plot_S506)
dev.off()
