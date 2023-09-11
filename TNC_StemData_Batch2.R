# Author: Emily Marie Purvis
# Date: 9.8.2023
# Goal: convert StemData_Batch2.xls (TNC) individual tree coordinates from distance/azimuth to easting/northing

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2")

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(pracma)
library(ggrepel)

#### Load and inspect data; small tweaks to get it standardized and ready for calculating tree coordinates ####

treedata = read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\Plot_Data\\StemData_Batch2.xlsx"),sheet=1,col_names = TRUE)

# largeplotdata = st_read("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_1\\large-plots_for-iri.kml")

# smallplotdata = st_read("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_1\\small-plots_for-iri_v2.kml")

# just kidding there are updated GPS coordinates of plot centers....

L502 <- read_sf('C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\Post_Processed_GPS\\L-502_g\\L-502.shp') 

L502 = L502 %>% mutate(`Plot#` = 'L502')

# st_crs(L502)
# EPSG 4326

L504 <- read_sf('C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\Post_Processed_GPS\\L-504_g\\L-504.shp') 

L504 = L504 %>% mutate(`Plot#` = 'L504')

L504 = L504 %>% st_set_crs(4326)

# not all shapefiles have a crs, so I'm checking manually and adding if necessary
# st_crs(L504)

L505 <- read_sf('C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\Post_Processed_GPS\\L-505_g\\L-505.shp') 

L505 = L505 %>% mutate(`Plot#` = 'L505')

L505 = L505 %>% st_set_crs(4326)

# st_crs(L505)

L507 <- read_sf('C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\Post_Processed_GPS\\L-507_g\\L-507.shp') 

L507 = L507 %>% mutate(`Plot#` = 'L507')

# st_crs(L507)

L508 <- read_sf('C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\Post_Processed_GPS\\L-508_g\\L-508.shp') 

L508 = L508 %>% mutate(`Plot#` = 'L508')

# st_crs(L508)

L521 <- read_sf('C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\Post_Processed_GPS\\L-521_g\\L-521.shp') 

L521 = L521 %>% mutate(`Plot#` = 'L521')

L521 = L521 %>% st_set_crs(4326)

# st_crs(L521)

L529 <- read_sf('C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\Post_Processed_GPS\\L-529_g\\L-529.shp') 

L529 = L529 %>% mutate(`Plot#` = 'L529')

# st_crs(L529)

L536 <- read_sf('C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\Post_Processed_GPS\\L-536_g\\L-536.shp') 

L536 = L536 %>% mutate(`Plot#` = 'L536')

# st_crs(L536)

S038 <- read_sf('C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\Post_Processed_GPS\\S-038_g\\S-038.shp') 

S038 = S038 %>% mutate(`Plot#` = 'S038')

# st_crs(S038)

S313 <- read_sf('C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\Post_Processed_GPS\\S-313_g\\S-313.shp') 

S313 = S313 %>% mutate(`Plot#` = 'S313')

# st_crs(S313)

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

# convert the plot center coordinates to UTM10N format so the calculations in the next step work. first, merge all plot centers into 1 spatial dataframe

# allplots <- rbind(largeplotdata, smallplotdata)

# plot center sf objects all have different columns bc of course they do, first need to reformat them so they all have the same columns

L502 <- L502[,-1:-19]

L504 <- L504[,-1:-18]

L505 <- L505[,-1:-18]

L507 <- L507[,-1:-19]

L508 <- L508[,-1:-19]

L521 <- L521[,-1:-18]

L529 <- L529[,-1:-18]

L536 <- L536[,-1:-19]

S038 <- S038[,-1:-19]

S313 <- S313[,-1:-18]

allplots <- rbind(L502, L504, L505, L507, L508, L521, L529, L536, S038, S313) 

allplots_UTM10N <- st_transform (allplots, 32610) 

# save the plot centers together in one spatial data file just in case it's useful later

st_write(allplots, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\Stem_Batch_2_updatedplotcenters.gpkg"),delete_dsn=TRUE)

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

st_write(trees_sp %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\Stem_Batch_2_treecoordinates_updatedplotcenters.gpkg"),delete_dsn=TRUE)

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

# this code is not in the excel data file. prunus emarginata (FIA code for 768) is in the area though so it's plausible.

trees_calcs$`Species`[trees_calcs$`Species` == '768'] <- 'PREM'

# guessing this one is a typo. 312 is box elder, and this site has too high elevation for box elders

trees_calcs$`Species`[trees_calcs$`Species` == '313'] <- 'ACMA3'

# guessing this is a typo too. 21 is shasta red fir, definitely out of range here

trees_calcs$`Species`[trees_calcs$`Species` == '21'] <- 'ABMA'

# want to extract WSG84 coordinates for each tree into x and y columns. do this by writing the dataframe back into a spatial dataframe, changing the coordinate system, and extracting the new coordinates

# just kidding I did it in UTM so the plots look like circles! keeping this code because it worked well though

# trees_sp_ggplot <- st_as_sf(trees_calcs, coords = c("TreeEasting","TreeNorthing"), crs = 32610) %>% st_transform(4326)

# trees_ggplot <- data.frame(trees_sp_ggplot, st_coordinates(trees_sp_ggplot[,1], st_coordinates(trees_sp_ggplot[,2]))) %>% rename (Easting=X, Northing=Y)

# futzing around with WSG84 plot L521

# ggplot(subset(trees_ggplot,`Plot.`=="L521"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 4)) + coord_equal(expand=FALSE) + theme_classic() + geom_text(color="black", size=3, vjust="top", hjust ="right") + xlim(-120.82525,-120.82225) + ylim (39.5045, 39.507) + ggtitle ("Plot L521") + theme(plot.title = element_text(hjust = 0.5)) 

# futzing around with UTM plot L521

# rearranging the trees in descending DBH order will hopefully help the smaller points be plotted on top

trees_calcs <- trees_calcs %>% rename (Easting=TreeEasting, Northing=TreeNorthing) %>% arrange(desc(DBH))

allplots_UTM10N <- allplots_UTM10N %>% rename (Easting=PlotEastingUTM10N, Northing=PlotNorthingUTM10N)

ggplot(subset(trees_calcs,`Plot#`=="L521"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot L521") + theme(plot.title = element_text(hjust = 0.5)) + xlim(686950, 687190) + ylim (4375050, 4375265) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() -> plot_L521


# trying to add plot center, need to keep working on this

# + geom_point (subset(allplots_UTM10N,`Plot#`=="L521"), mapping= aes(size=3, color="black"))

# want to streamline the process quite a lot to write a function that does all the plots, but I'm struggling to do so because the automatically generated x and y axis limits are weirdly bad and exclude data points around the margins. This means manually adjusting the x and y limits for each graph seems like the way to go. In the future, dig deeper to figure out how to automatically add a certain amount of area around all the points!

# plot L505

ggplot(subset(trees_calcs,`Plot#`=="L505"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right", max.overlaps = Inf) + ggtitle ("Plot L505") + theme(plot.title = element_text(hjust = 0.5)) + xlim(682250, 682465) + ylim (4390425, 4390640) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() -> plot_L505

# plot L504

ggplot(subset(trees_calcs,`Plot#`=="L504"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right", max.overlaps = Inf) + ggtitle ("Plot L504") + theme(plot.title = element_text(hjust = 0.5)) + xlim(683185, 683400) + ylim (4390585, 4390800) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() -> plot_L504

# plot L536

ggplot(subset(trees_calcs,`Plot#`=="L536"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right", max.overlaps = Inf) + ggtitle ("Plot L536") + theme(plot.title = element_text(hjust = 0.5)) + xlim(679755, 679975) + ylim (4386445, 4386660) + geom_point(shape=1, aes(size=DBH), colour = "black") + scale_colour_viridis_d() -> plot_L536

# okay THIS label situation needs to be improved, what is going on in the lower left corner

# plot L507

ggplot(subset(trees_calcs,`Plot#`=="L507"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right", max.overlaps = Inf) + ggtitle ("Plot L507") + theme(plot.title = element_text(hjust = 0.5)) + xlim(680525, 680750) + ylim (4385820, 4386010) + geom_point(shape=1, aes(size=DBH), colour = "black") + scale_colour_viridis_d() -> plot_L507

# plot L502

ggplot(subset(trees_calcs,`Plot#`=="L502"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right", max.overlaps = Inf) + ggtitle ("Plot L502") + theme(plot.title = element_text(hjust = 0.5)) + xlim(679475, 679700) + ylim (4385500, 4385715) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() -> plot_L502

# plot S038

ggplot(subset(trees_calcs,`Plot#`=="S038"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S038") + theme(plot.title = element_text(hjust = 0.5)) + xlim(674680, 674790) + ylim (4380915, 4381020) + geom_point(shape=1, aes(size=DBH), colour = "black") + scale_colour_viridis_d() -> plot_S038

# plot L529

ggplot(subset(trees_calcs,`Plot#`=="L529"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot L529") + theme(plot.title = element_text(hjust = 0.5)) + xlim(669950, 670165) + ylim (4376810, 4377005) + geom_point(shape=1, aes(size=DBH), colour = "black") + scale_colour_viridis_d() -> plot_L529

# plot L508

ggplot(subset(trees_calcs,`Plot#`=="L508"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right", max.overlaps = Inf) + ggtitle ("Plot L508") + theme(plot.title = element_text(hjust = 0.5)) + xlim(672780, 673000) + ylim (4379140, 4379340) + geom_point(shape=1, aes(size=DBH), colour = "black") + scale_colour_viridis_d() -> plot_L508

# plot S313

ggplot(subset(trees_calcs,`Plot#`=="S313"), aes(Easting, Northing, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot S313") + theme(plot.title = element_text(hjust = 0.5)) + xlim(670200, 670305) + ylim (4378510, 4378625) + geom_point(shape=1, aes(size=DBH), colour = "black") + scale_colour_viridis_d() -> plot_S313

# export plots to pdf

pdf(file="C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\StemMap_ggplots_updatedplotcenters.pdf",height = 8, width = 8)
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


