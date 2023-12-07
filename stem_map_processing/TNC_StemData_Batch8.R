# Author: Emily Marie Purvis
# Date: 12.6.2023
# Goal: convert Batch 8 individual tree coordinates from distance/azimuth to easting/northing and create stem map ggplots

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_8\\Batch_8")

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(pracma)
library(ggrepel)

#### Load and inspect tree data; small tweaks to get it standardized and ready for calculating tree coordinates ####

treedata = read_excel(data("Plot_Data\\Stem_Batch_8.xlsx"),sheet=1,col_names = TRUE)

# need to reformat the plot numbers in treedata so they match the plot data 

treedata$`Plot #`[treedata$`Plot #` == 'S-023'] <- 'S023'
treedata$`Plot #`[treedata$`Plot #` == 'S-028'] <- 'S028'
treedata$`Plot #`[treedata$`Plot #` == 'S-032'] <- 'S032'
treedata$`Plot #`[treedata$`Plot #` == 'S-079'] <- 'S079'
treedata$`Plot #`[treedata$`Plot #` == 'S-302'] <- 'S302'
treedata$`Plot #`[treedata$`Plot #` == 'S-308'] <- 'S308'
treedata$`Plot #`[treedata$`Plot #` == 'S-310'] <- 'S310'
treedata$`Plot #`[treedata$`Plot #` == 'S-903'] <- 'S903'
treedata$`Plot #`[treedata$`Plot #` == 'S-945'] <- 'S945'
treedata$`Plot #`[treedata$`Plot #` == 'S-960'] <- 'S960'

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

plotcenters <- read_sf ("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_8\\Stem_Batch_8_plotcoordinatesWGS84.gpkg")

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

st_write(trees_sp %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_8\\Stem_Batch_8_treecoordinatesWGS84.gpkg"),delete_dsn=TRUE)

st_write(trees_sp %>% st_transform(32610),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_8\\Stem_Batch_8_treecoordinatesUTM10N.gpkg"),delete_dsn=TRUE)

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

# plot S023

ggplot(subset(all_points,`Plot #`=="S023"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S023") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(670950, 671075) + ylim (4380775, 4380895) -> plot_S023

# plot S028

ggplot(subset(all_points,`Plot #`=="S028"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S028") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(672920, 673015) + ylim (4378055, 4378165) -> plot_S028

# plot S032

ggplot(subset(all_points,`Plot #`=="S032"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S032") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(673825, 673940) + ylim (4378095, 4378220) -> plot_S032

# plot S079

ggplot(subset(all_points,`Plot #`=="S079"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S079") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(683600, 683725) + ylim (4391370, 4391490) -> plot_S079

# plot S302

ggplot(subset(all_points,`Plot #`=="S302"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S302") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(706795, 706860) + ylim (4391215, 4391315) -> plot_S302

# plot S308

ggplot(subset(all_points,`Plot #`=="S308"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S308") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(716190, 716310) + ylim (4380510, 4380605) -> plot_S308

# plot S310

ggplot(subset(all_points,`Plot #`=="S310"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S310") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(675605, 675690) + ylim (4382200, 4382270) -> plot_S310

# plot S903

ggplot(subset(all_points,`Plot #`=="S903"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S903") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(683760, 683845) + ylim (4392960, 4393080) -> plot_S903

# plot S945

ggplot(subset(all_points,`Plot #`=="S945"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S945") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(684225, 684355) + ylim (4392980, 4393105) -> plot_S945

# plot S960

ggplot(subset(all_points,`Plot #`=="S960"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S960") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(682400, 682515) + ylim (4387945, 4388030) -> plot_S960

# export plots to pdf

pdf(file="C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_8\\StemMap8_ggplots.pdf",height = 8, width = 8)
plot(plot_S023)
plot(plot_S028)
plot(plot_S032)
plot(plot_S079)
plot(plot_S302)
plot(plot_S308)
plot(plot_S310)
plot(plot_S903)
plot(plot_S945)
plot(plot_S960)
dev.off()

