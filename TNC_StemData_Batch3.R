# Author: Emily Marie Purvis
# Date: 9.11.2023
# Goal: convert StemData_Batch3.xls (TNC) individual tree coordinates from distance/azimuth to easting/northing

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_3")

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(pracma)
library(ggrepel)

#### Load and inspect data; small tweaks to get it standardized and ready for calculating tree coordinates ####

treedata = read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_3\\StemData_Batch3.xlsx"),sheet=1,col_names = TRUE)

largeplotdata = st_read("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_1\\large-plots_for-iri.kml")

smallplotdata = st_read("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_1\\small-plots_for-iri_v2.kml")

# st_crs(largeplotdata)
# st_crs(smallplotdata)
# both plot data sets in EPSG 4326

# need to reformat the plot numbers in treedata so they match the plot data 

treedata$`Plot#`[treedata$`Plot#` == 'L-519'] <- 'L519'
treedata$`Plot#`[treedata$`Plot#` == 'S-087'] <- 'S087'
treedata$`Plot#`[treedata$`Plot#` == 'S-084'] <- 'S084'
treedata$`Plot#`[treedata$`Plot#` == 'S-080'] <- 'S080'
treedata$`Plot#`[treedata$`Plot#` == 'S-083'] <- 'S083'
treedata$`Plot#`[treedata$`Plot#` == 'S-097'] <- 'S097'
treedata$`Plot#`[treedata$`Plot#` == 'S-038'] <- 'S038'
treedata$`Plot#`[treedata$`Plot#` == 'S-075'] <- 'S075'
treedata$`Plot#`[treedata$`Plot#` == 'S-973'] <- 'S973'
treedata$`Plot#`[treedata$`Plot#` == 'S-015'] <- 'S015'

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

# convert the plot center coordinates to UTM10N format so the calculations in the next step work. first, merge all plot centers into 1 spatial dataframe

allplots <- rbind(largeplotdata, smallplotdata)

allplots_UTM10N <- st_transform (allplots, 32610) %>% rename ('Plot#'=Name)

# extract the geometry features into columns of lat and long, then merge back into the spatial data frame

UTM10N_plotcoords <- data.frame(allplots_UTM10N$'Name', st_coordinates(allplots_UTM10N[,1], st_coordinates(allplots_UTM10N[,2]))) %>% rename ('Plot#'=allplots_UTM10N.Name, PlotEastingUTM10N=X, PlotNorthingUTM10N=Y)

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

st_write(trees_sp %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_3\\Stem_Batch_3_treecoordinates.gpkg"),delete_dsn=TRUE)

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

# guessing tree codes vs typos







# rearranging the trees in descending DBH order will hopefully help the smaller points be plotted on top

trees_calcs <- trees_calcs %>% rename (Easting=TreeEasting, Northing=TreeNorthing) %>% arrange(desc(DBH))

# getting plot centers & trees in one data frame will make plotting easier

allplots_UTM10N <- allplots_UTM10N %>% rename (Easting=PlotEastingUTM10N, Northing=PlotNorthingUTM10N)

all_trees <- as.data.frame(trees_calcs[,c('Plot#','Easting','Northing', 'Species', 'DBH', 'tree_id')])

allplots_UTM10N$Species <- '.Plot Center'

allplots_UTM10N$DBH <- 10

allplots_UTM10N$tree_id <- 'Plot Center'

all_plots <- as.data.frame(allplots_UTM10N [,c('Plot#','Easting','Northing', 'Species', 'DBH', 'tree_id')])

all_plots <- subset(all_plots, select = -geometry)

all_points <- rbind(all_plots, all_trees)

# plot L519

ggplot(subset(all_points,`Plot#`=="L519"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot L519") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(677990, 678210) + ylim (4384845, 4385055)  -> plot_L519

# plot S074

ggplot(subset(all_points,`Plot#`=="S074"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S074") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(682740, 682860) + ylim (4389510, 4389630) -> plot_S074

# plot S087

ggplot(subset(all_points,`Plot#`=="S087"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right", max.overlaps = Inf) + ggtitle ("Plot S087") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(687800, 687905) + ylim (4376700, 4376825)-> plot_S087

# plot S084

ggplot(subset(all_points,`Plot#`=="S084"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right", max.overlaps = Inf) + ggtitle ("Plot S084") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black") + scale_colour_viridis_d() + xlim(685905, 686025) + ylim (4377570, 4377690) -> plot_S084

# plot S080

ggplot(subset(all_points,`Plot#`=="S080"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right", max.overlaps = Inf) + ggtitle ("Plot S080") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black") + scale_colour_viridis_d() + xlim(685010, 685135) + ylim (4376615, 4376735)-> plot_S080

# plot S083

ggplot(subset(all_points,`Plot#`=="S083"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right", max.overlaps = Inf) + ggtitle ("Plot S083") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(686050, 686155) + ylim (4373865, 4373990) -> plot_S083

# plot S097

ggplot(subset(all_points,`Plot#`=="S097"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S097") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black") + scale_colour_viridis_d() + xlim(694340, 694440) + ylim (4375990, 4376100) -> plot_S097

# plot S075

ggplot(subset(all_points,`Plot#`=="S075"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S075") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black") + scale_colour_viridis_d()  + xlim(682700, 682820) + ylim (4390440, 4390555) -> plot_S075

# plot S973

ggplot(subset(all_points,`Plot#`=="S973"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right", max.overlaps = Inf) + ggtitle ("Plot S973") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black") + scale_colour_viridis_d() + xlim(664865, 664970) + ylim (4373330, 4373450) -> plot_S973

# plot S015

ggplot(subset(all_points,`Plot#`=="S015"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S015") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black") + scale_colour_viridis_d() + xlim(667485, 667610) + ylim (4373255, 4373375) -> plot_S015

# export plots to pdf

pdf(file="C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_3\\StemMap_ggplots.pdf",height = 8, width = 8)
plot(plot_L519)
plot(plot_S074)
plot(plot_S087)
plot(plot_S084)
plot(plot_S080)
plot(plot_S083)
plot(plot_S097)
plot(plot_S075)
plot(plot_S973)
plot(plot_S015)
dev.off()


