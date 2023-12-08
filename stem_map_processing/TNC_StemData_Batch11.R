# Author: Emily Marie Purvis
# Date: 12.6.2023
# Goal: convert Batch 11 individual tree coordinates from distance/azimuth to easting/northing and create stem map ggplots

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_11\\Batch_11_Final")

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(pracma)
library(ggrepel)

#### Load and inspect tree data; small tweaks to get it standardized and ready for calculating tree coordinates ####

treedata = read_excel(data("Batch_11.xlsx"),sheet=1,col_names = TRUE)

# need to reformat the plot numbers in treedata so they match the plot data 

treedata <- treedata %>% rename("Plot #" = `Plot#`)

treedata$`Plot #`[treedata$`Plot #` == 'S076'] <- 'S076'

# need to delete random rows of all NAs in treedata

treedata <- treedata %>% drop_na(`Plot #`)

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

plotcenters <- read_sf ("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_11\\Stem_Batch_11_plotcoordinatesWGS84.gpkg")

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

st_write(trees_sp %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_11\\Stem_Batch_11_treecoordinatesWGS84.gpkg"),delete_dsn=TRUE)

st_write(trees_sp %>% st_transform(32610),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_11\\Stem_Batch_11_treecoordinatesUTM10N.gpkg"),delete_dsn=TRUE)

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
treedata$`Species`[treedata$`Species` == '112'] <- 'PIPO' # 112 is pinus engelmannii, almost certainly a typo

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

# plot S001

ggplot(subset(all_points,`Plot #`=="S001"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S001") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(660125, 660240) + ylim (4371140, 4371260) -> plot_S001

# plot S007

ggplot(subset(all_points,`Plot #`=="S007"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S007") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(662785, 662900) + ylim (4374980, 4375100) -> plot_S007

# plot S008

ggplot(subset(all_points,`Plot #`=="S008"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S008") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(662760, 662875) + ylim (4375890, 4376015) -> plot_S008

# plot S058

ggplot(subset(all_points,`Plot #`=="S058"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S058") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(680055, 680170) + ylim (4386670, 4386760) -> plot_S058

# plot S065

ggplot(subset(all_points,`Plot #`=="S065"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S065") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(680960, 681050) + ylim (4387580, 4387690) -> plot_S065

#plot S070

ggplot(subset(all_points,`Plot #`=="S070"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S070") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(681875, 682000) + ylim (4387640, 4387740) -> plot_S070

# plot S076

ggplot(subset(all_points,`Plot #`=="S076"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S076") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(683695, 683820) + ylim (4388610, 4388735) -> plot_S076

# plot S307

ggplot(subset(all_points,`Plot #`=="S307"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S307") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(661795, 661880) + ylim (4375720, 4375850) -> plot_S307

# plot S505

ggplot(subset(all_points,`Plot #`=="S505"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S505") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(677920, 678035) + ylim (4372300, 4372415) -> plot_S505

# plot S940

ggplot(subset(all_points,`Plot #`=="S940"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S940") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(680265, 680385) + ylim (4376330, 4376440) -> plot_S940

# export plots to pdf

pdf(file="C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_11\\StemMap11_ggplots.pdf",height = 8, width = 8)
plot(plot_S001)
plot(plot_S007)
plot(plot_S008)
plot(plot_S058)
plot(plot_S065)
plot(plot_S070)
plot(plot_S076)
plot(plot_S307)
plot(plot_S505)
plot(plot_S940)
dev.off()
