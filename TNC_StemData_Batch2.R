# Author: Emily Marie Purvis
# Date: 9.8.2023
# Goal: convert StemData_Batch2.xls (TNC) individual tree coordinates from distance/azimuth to easting/northing

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2")

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(dplyr)
library(pracma)
library(ggrepel)

#### Load and inspect data; small tweaks to get it standardized and ready for calculating tree coordinates ####

treedata = read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\Plot_Data\\StemData_Batch2.xlsx"),sheet=1,col_names = TRUE)

largeplotdata = st_read("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_1\\large-plots_for-iri.kml")

smallplotdata = st_read("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_1\\small-plots_for-iri_v2.kml")

# need to reformat the plot numbers in treedata so they match the plot data 

treedata$`Plot#`[treedata$`Plot#` == 'L-521'] <- 'L521'
treedata$`Plot#`[treedata$`Plot#` == 'L-505'] <- 'L505'
treedata$`Plot#`[treedata$`Plot#` == 'L-504'] <- 'L504'
treedata$`Plot#`[treedata$`Plot#` == 'L-536'] <- 'L536'
treedata$`Plot#`[treedata$`Plot#` == 'L-507'] <- 'L507'
treedata$`Plot#`[treedata$`Plot#` == 'L-502'] <- 'L502'
treedata$`Plot#`[treedata$`Plot#` == 'S-038'] <- 'S038'
treedata$`Plot#`[treedata$`Plot#` == 'L-529'] <- 'L529'
treedata$`Plot#`[treedata$`Plot#` == 'L-508'] <- 'L508'
treedata$`Plot#`[treedata$`Plot#` == 'S-313'] <- 'S313'

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

# st_crs (largeplotdata)
# st_crs (smallplotdata)
# EPSG of the plot data is 4326

# convert the plot center coordinates to UTM10N format so the calculations in the next step work. first, merge smallplotdata and largeplotdata into 1 spatial dataframe

allplots <- rbind(largeplotdata, smallplotdata)

allplots_UTM10N <- st_transform (allplots, 32610) %>% rename('Plot#'='Name')

# extract the geometry features into columns of lat and long, then merge back into the spatial data frame

UTM10N_plotcoords <- data.frame(allplots_UTM10N$'Plot#', st_coordinates(allplots_UTM10N[,1], st_coordinates(allplots_UTM10N[,2]))) %>% rename ('Plot#'=allplots_UTM10N..Plot.., PlotEastingUTM10N=X, PlotNorthingUTM10N=Y)

allplots_UTM10N <- full_join(allplots_UTM10N, UTM10N_plotcoords, by="Plot#")

#### Calculate the coordinates of each tree ####

# add new columns to the treedata dataframe so that each individual tree has a populated plot center easting and northing

trees_locations = full_join(treedata,st_drop_geometry(allplots_UTM10N),by='Plot#') %>% drop_na('HorizontalDistance')

# calculate the coordinates of each tree using the plot center, horizontal distance, and azimuth 

trees_calcs = trees_locations %>%
  mutate(TreeEasting = as.numeric(PlotEastingUTM10N) + sin(deg2rad(AZM)) * HorizontalDistance,
         TreeNorthing = as.numeric(PlotNorthingUTM10N) + cos(deg2rad(AZM)) * HorizontalDistance)

# give trees ID numbers

trees_calcs = trees_calcs %>% mutate(tree_id = 1:nrow(trees_calcs))

#### convert to spatial data and export ####

trees_sp <- st_as_sf(trees_calcs, coords = c("TreeEasting","TreeNorthing"), crs = 32610)

# exporting the tree spatial data in the same format as the plot data came in, 4326

st_write(trees_sp %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\Stem_Batch_2_treecoordinates.gpkg"),delete_dsn=TRUE)

#### ggplots ####

# rename species-- realizing after the fact I should have done this a shorter and more elegant way, a lesson for the future

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

# want to extract WSG84 coordinates for each tree into x and y columns. do this by writing the dataframe back into a spatial dataframe, changing the coordinate system, and extracting the new coordinates

trees_sp_ggplot <- st_as_sf(trees_calcs, coords = c("TreeEasting","TreeNorthing"), crs = 32610) %>% st_transform(4326)

trees_ggplot <- data.frame(trees_sp_ggplot, st_coordinates(trees_sp_ggplot[,1], st_coordinates(trees_sp_ggplot[,2]))) %>% rename (Easting=X, Northing=Y)

# futzing around with plot L521

ggplot(subset(trees_ggplot,`Plot.`=="L521"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 4)) + coord_equal(expand=FALSE) + theme_classic() + geom_text(color="black", size=3, vjust="top", hjust ="right") + xlim(-120.82525,-120.82225) + ylim (39.5045, 39.507) + ggtitle ("Plot L521") + theme(plot.title = element_text(hjust = 0.5)) 

# futzing around with UTM plot L521

# rearranging the trees in descending DBH order will hopefully help the smaller points be plotted on top

trees_calcs <- trees_calcs %>% rename (Easting=TreeEasting, Northing=TreeNorthing) %>% arrange(desc(DBH))

ggplot(subset(trees_calcs,`Plot#`=="L521"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=3, vjust="outward", hjust ="right") + xlim(686975, 687225) + ylim (4375050, 4375275) + ggtitle ("Plot L521") + theme(plot.title = element_text(hjust = 0.5)) -> plot_L521

# trying to add plot center, need to keep working on this

# + geom_point (data=subset(allplots_UTM10N,`Plot#`=="L521"), aes(x=PlotEastingUTM10N, y=PlotNorthingUTM10N), color="black")

# also want to make the labels not overlap-- when I use the built in "check overlap" ggplot2 function it just makes some of the labels disappear!

# want to streamline the process quite a lot to write a function that does all the plots, but I'm struggling to do so because the automatically generated x and y axis limits are weirdly bad and exclude data points around the margins. This means manually adjusting the x and y limits for each graph seems like the way to go. In the future, dig deeper to figure out how to automatically add a certain amount of area around all the points!

# plot L505

ggplot(subset(trees_calcs,`Plot#`=="L505"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=3, vjust="outward", hjust ="right", max.overlaps = Inf) + ggtitle ("Plot L505") + theme(plot.title = element_text(hjust = 0.5)) + xlim(682250, 682475) + ylim (4390425, 4390650) -> plot_L505

# oh dear the labels on L505 are so bad

# plot L504

ggplot(subset(trees_calcs,`Plot#`=="L504"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=3, vjust="outward", hjust ="right", max.overlaps = Inf) + ggtitle ("Plot L504") + theme(plot.title = element_text(hjust = 0.5)) + xlim(683175, 683400) + ylim (4390575, 4390800) -> plot_L504

# the label situation is getting worse with every plot...

# plot L536

ggplot(subset(trees_calcs,`Plot#`=="L536"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=3, vjust="outward", hjust ="right", max.overlaps = Inf) + ggtitle ("Plot L536") + theme(plot.title = element_text(hjust = 0.5)) + xlim(679775, 680025) + ylim (4386425, 4386650) -> plot_L536

# the label situation is now comically bad

# plot L507

ggplot(subset(trees_calcs,`Plot#`=="L507"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=3, vjust="outward", hjust ="right", max.overlaps = Inf) + ggtitle ("Plot L507") + theme(plot.title = element_text(hjust = 0.5)) + xlim(680525, 680750) + ylim (4385800, 4386025) -> plot_L507

# plot L502

ggplot(subset(trees_calcs,`Plot#`=="L502"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=3, vjust="outward", hjust ="right") + ggtitle ("Plot L502") + theme(plot.title = element_text(hjust = 0.5)) + xlim(679500, 679725) + ylim (4385500, 4385725) -> plot_L502

# plot S038

ggplot(subset(trees_calcs,`Plot#`=="S038"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=3, vjust="outward", hjust ="right") + ggtitle ("Plot S038") + theme(plot.title = element_text(hjust = 0.5)) + xlim(674650, 674775) + ylim (4380925, 4381025) -> plot_S038

# finally one that looks okay! all hail small plot

# plot L529

ggplot(subset(trees_calcs,`Plot#`=="L529"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=3, vjust="outward", hjust ="right") + ggtitle ("Plot L529") + theme(plot.title = element_text(hjust = 0.5)) + xlim(669950, 670175) + ylim (4376810, 4377025) -> plot_L529

# plot L508

ggplot(subset(trees_calcs,`Plot#`=="L508"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=3, vjust="outward", hjust ="right", max.overlaps = Inf) + ggtitle ("Plot L508") + theme(plot.title = element_text(hjust = 0.5)) + xlim(672810, 673040) + ylim (4379100, 4379315) -> plot_L508

# another terrible label priority

# plot S313

ggplot(subset(trees_calcs,`Plot#`=="S313"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=3, vjust="outward", hjust ="right") + ggtitle ("Plot S313") + theme(plot.title = element_text(hjust = 0.5)) + xlim(670190, 670300) + ylim (4378510, 4378630) -> plot_S313

# export plots to pdf

pdf(file="C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\StemMap_ggplots.pdf",height = 8, width = 8)
plot(plot_L521)
plot(plot_L505)
plot(plot_L504)
plot(plot_L536)
plot(plot_L507)
plot(plot_L502)
plot(plot_S038)
plot(plot_L529)
plot(plot_L508)
plot(plot_S313)
dev.off()


