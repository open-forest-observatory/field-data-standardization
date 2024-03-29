# Author: Emily Marie Purvis
# Date: 1.14.2023
# Goal: convert Batch 15 individual tree coordinates from distance/azimuth to easting/northing, get this data in the same format as the rest of the IRI data

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_L527\\L-527")

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(pracma)
library(ggrepel)

#### Load and inspect tree data; small tweaks to get it standardized and ready for calculating tree coordinates ####

treedata = read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_L527\\L-527\\Data\\L-527.xlsx"),sheet=1,col_names = TRUE)

# need to reformat the plot numbers in treedata so they match the plot data 

treedata$`Plot #`[treedata$`Plot #` == 'L-527'] <- 'L527'

# need to delete random rows of all NAs in treedata

treedata <- treedata %>% drop_na(`Plot #`)

# R and I both get easily confused about the % slope column so I'm renaming it

treedata <- treedata %>% rename("PercentSlope" = `% slope`)

#### Load and inspect plot gps data ####

# Import plot coordinates

L527 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_L527\\L-527\\Post_Processed_GPS\\L527\\L527.shp") 

L527= L527 %>% mutate(Name = 'L527')

L527 <- L527 [-c(1:19, 22:23)]

# st_crs(L527)
# EPSG 4326

BatchL527plotsWGS84 <- L527 [-c(1:2)]

st_write(BatchL527plotsWGS84, data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_L527\\Stem_Batch_L527_plotcoordinatesWGS84.gpkg"),delete_dsn=TRUE)

plotcenter <- BatchL527plotsWGS84 %>% rename(`Plot #` = `Name`)

plotcenter_UTM10N <- st_transform (plotcenter, 32610) 

# extract the geometry features into columns of lat and long, then merge back into the spatial data frame

UTM10N_plotcoords <- data.frame(plotcenter_UTM10N$'Plot #', st_coordinates(plotcenter_UTM10N[,1], st_coordinates(plotcenter_UTM10N[,2]))) 

UTM10N_plotcoords <- UTM10N_plotcoords %>% rename ('Plot #'=plotcenter_UTM10N..Plot..., PlotLongitudeUTM10N=X, PlotLatitudeUTM10N=Y)

UTM10N_plotcoords <- UTM10N_plotcoords [-c(4:5)]

plotcenter_UTM10N <- full_join(plotcenter_UTM10N, UTM10N_plotcoords, by="Plot #")

#### Calculate the coordinates of each tree ####

# add new columns to the treedata dataframe so that each individual tree has a populated plot center easting and northing

treedata = full_join(treedata,st_drop_geometry(plotcenter_UTM10N),by='Plot #')

# calculate the coordinates of each tree using the plot center, horizontal distance, and azimuth 

treedata = treedata %>%
  mutate(TreeLongitudeUTM10N = as.numeric(PlotLongitudeUTM10N) + sin(deg2rad(AZM)) * (`H Distance`* 0.3048),
         TreeLatitudeUTM10N = as.numeric(PlotLatitudeUTM10N) + cos(deg2rad(AZM)) * (`H Distance`) * 0.3048)

# give trees ID numbers

treedata = treedata %>% mutate(tree_id = 1:nrow(treedata))

#### convert to spatial data and export ####

trees_sp <- st_as_sf(treedata, coords = c("TreeLongitudeUTM10N","TreeLatitudeUTM10N"), crs = 32610)

# exporting the tree spatial data in the same format as the plot data came in, 4326

st_write(trees_sp %>% st_transform(4326),data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_L527\\Stem_Batch_L527_treecoordinatesWGS84.gpkg"),delete_dsn=TRUE)

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

plotcenter_UTM10N <- st_drop_geometry(plotcenter_UTM10N) %>% rename (Longitude=PlotLongitudeUTM10N, Latitude=PlotLatitudeUTM10N)

all_trees <- as.data.frame(treedata[,c('Plot #','Longitude','Latitude', 'Species', 'DBH', 'tree_id')])

plotcenter_UTM10N$Species <- 'Plot Center'

plotcenter_UTM10N$DBH <- 0

plotcenter_UTM10N$tree_id <- 'Plot Center'

all_plots <- as.data.frame(plotcenter_UTM10N [,c('Plot #','Longitude','Latitude', 'Species', 'DBH', 'tree_id')])

all_points <- rbind(all_plots, all_trees)

# plot L527

ggplot(subset(all_points,`Plot #`=="L527"), aes(Longitude, Latitude, color=Species, label=tree_id)) + geom_point(aes(size=DBH)) + scale_size_continuous(range = c(1, 6)) + coord_equal(expand=FALSE) + theme_classic() + geom_text_repel(color="black", size=2.5, vjust="outward", hjust ="right") + ggtitle ("Plot L527") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(shape=1, aes(size=DBH), colour = "black")  + scale_colour_viridis_d() + xlim(668805, 668875) + ylim (4365095, 4365160) -> plot_L527


# export plots to pdf

pdf(file="C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_L527\\StemMapL527_ggplots.pdf",height = 8, width = 8)
plot(plot_L527)
dev.off()
