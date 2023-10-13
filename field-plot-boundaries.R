# Author: Emily Marie Purvis
# Date: 10.2.2023
# Goal: Write code that creates polygons around plot centers and exports them to .gpkg files

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data")

#### Load libraries ####
library(sf)
library(tidyverse)

#### VP ctlsierra2022 dataset ####

#### Load data ####

plotdata = read.csv("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\1_received-data\\0001\\data\\data\\vp_ctlsierra2022_v1_plots.csv", header = TRUE)

# Remember to double check the CRS! This VP data is in WGS84, which is what we want

# we don't want all of these columns, just plot ID and spatial attributes

plotdata = subset(plotdata, select = c('plot_id','lon', 'lat') )

# change the title of the contributor's plot ID column

plotdata <- plotdata %>% rename("contributor_id" = "plot_id")

# add new plot ID column for OFO IDs

plotdata <- plotdata %>% add_column(plot_id = "")

# adding in the OFO plot_ids

# a way faster way of doing this

plotdata$plot_id <- 1:nrow(plotdata)

plotdata$plot_id <- formatC(as.numeric(plotdata$plot_id), width = 4, format = "d", flag = "0")

# future datasets can use something like plotdata$plot_id <- (1:nrow(plotdata) + 51)

# remove contributor id column

plotdata = subset(plotdata, select = c('plot_id','lon', 'lat') )

# convert to sf

plotdata_sf <- st_as_sf(plotdata, coords = c("lon", "lat"), crs = 4326)

# These are circular plots! Buffer circles by 100 feet (30.48 meters)-- this is the plot radius
# First project to a projected (meters) coordinate system with equal x and y distances
# CONUS Albers Equal Area (EPSG: 5070) covers the CONUS, but will need a different one for any future plots outside the CONUS
plotdata_sf = st_transform(plotdata_sf, crs = 5070)
plotdata_circles <- st_buffer(plotdata_sf, dist = 30.48, nQuadSegs = 10)
# Then back to WGS84
plotdata_circles <- st_transform(plotdata_circles, crs = 4326)


# First project to a projected (meters) coordinate system with equal x and y distances

# CONUS Albers Equal Area (EPSG: 5070) covers the CONUS, but will need a different one for any future plots outside the CONUS

plotdata_sf = st_transform(plotdata_sf, crs = 5070)

plotdata_circles <- st_buffer(plotdata_sf, dist = 30.48, nQuadSegs = 10)

# Then back to WGS84

plotdata_circles <- st_transform(plotdata_circles, crs = 4326)


#### Need to export each plot polygon individually ####

for(i in 1:nrow(plotdata_circles)) {
  polygon_current <- plotdata_circles[i, ]
  plot_id_current = polygon_current$plot_id
  st_write(polygon_current, paste0("ofo-field-data\\2_standardized-data\\field-plot-boundaries\\", plot_id_current, ".gpkg"))
}


#### FOCAL dispersal project datset ####

# this one is a bit different because we have polygons already! we do need to save individual polygon .gpkg files, and we do need to calculate the area and centroid of each polygon

#### Calculate area of each polygon ####

polygons <- st_read("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\1_received-data\\0004\\data\\plot_bounds_v3_manuallyCorrected.gpkg", crs = 4326)

# First project to a projected (meters) coordinate system with equal x and y distances, CONUS Albers Equal Area (EPSG: 5070) 

polygons = st_transform(polygons, crs = 5070)

st_area (polygons)

# Add a new column to the spatial data frame for area and calculate

polygons$area_meters <- 0

polygons$area_meters = st_area (polygons)

# Convert back to 4326

polygons = st_transform(polygons, crs = 4326)

#### Calculate centroid of each polygon ####

polygons$centroid <- 0

polygons$centroid <- st_centroid(polygons)

#### Export each plot polygon individually ####

# First need to add OFO plot IDs

polygons <- polygons %>%
  add_column(plot_id_ofo = "0")

polygons$plot_id_ofo[polygons$stem_map_name == 'Chips_1_ABCO'] <- '0057'
polygons$plot_id_ofo[polygons$stem_map_name == 'Chips_1'] <- '0058'
polygons$plot_id_ofo[polygons$stem_map_name == 'Valley_1'] <- '0059'
polygons$plot_id_ofo[polygons$stem_map_name == 'Lassic_1'] <- '0060'
polygons$plot_id_ofo[polygons$stem_map_name == 'Delta_1'] <- '0061'
polygons$plot_id_ofo[polygons$stem_map_name == 'Lassic_2'] <- '0062'
polygons$plot_id_ofo[polygons$stem_map_name == 'Delta_2'] <- '0063'
polygons$plot_id_ofo[polygons$stem_map_name == 'Chips_2'] <- '0064'
polygons$plot_id_ofo[polygons$stem_map_name == 'Delta_3'] <- '0065'
polygons$plot_id_ofo[polygons$stem_map_name == 'Creek_1'] <- '0066'
polygons$plot_id_ofo[polygons$stem_map_name == 'Creek_2'] <- '0067'

# We don't want all of the columns in the dataset, just plot ID and spatial attributes

polygons = subset(polygons, select = c('plot_id_ofo','geom') )

# Export

for(i in 1:nrow(polygons)) {
  polygon_current <- polygons[i, ]
  plot_id_current = polygon_current$plot_id_ofo
  st_write(polygon_current, paste0("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\2_standardized-data\\field-plot-boundaries\\", plot_id_current, ".gpkg"))
}

#### SSI Grass Valley OnePlot ####


# make the plot center coordinates into a spatial dataframe

SSIplotcenter <- matrix(c('1', -120.905293, 39.199336), ncol=3, byrow=TRUE)

colnames(SSIplotcenter) <- c('PLOT_ID','PlotCenterEasting','PlotCenterNorthing')

SSIplotcenter <- as.data.frame.matrix(SSIplotcenter)

SSIplotcenter_sp <- st_as_sf(SSIplotcenter, coords = c("PlotCenterEasting", "PlotCenterNorthing"), crs = 4326, remove=F)

# These are circular plots! Buffer circles by 100 feet (30.48 meters)-- this is the plot radius

# First project to a projected (meters) coordinate system with equal x and y distances

# CONUS Albers Equal Area (EPSG: 5070) covers the CONUS, but will need a different one for any future plots outside the CONUS

SSIplotcenter_sp = st_transform(SSIplotcenter_sp, crs = 5070)

SSIplotcenter_circles <- st_buffer(SSIplotcenter_sp, dist = 30.48, nQuadSegs = 10)

# Then back to WGS84

SSIplotcenter_circles <- st_transform(SSIplotcenter_circles, crs = 4326)

# add OFO plot ID

SSIplotcenter_circles <- SSIplotcenter_circles %>%
  add_column(plot_id_ofo = "0052")

# pare down spatial data file so it only contains geometry and OFO plot ID

SSIplotcenter_circles = subset(SSIplotcenter_circles, select = c('plot_id_ofo','geometry') )

# Export polygon

st_write(SSIplotcenter_circles, "C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\2_standardized-data\\field-plot-boundaries\\0052.gpkg")

#### Weeks Emerald Point ####

# import

plotpolygon <- st_read("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\1_received-data\\0005\\data\\ground_map_mask_precise.geojson")

# add OFO plot ID

plotpolygon <- plotpolygon %>%
  add_column(plot_id_ofo = "0068")

# Export polygon

st_write(plotpolygon, "C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\2_standardized-data\\field-plot-boundaries\\0068.gpkg")
