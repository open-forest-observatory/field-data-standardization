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

polygons <- st_read("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\1_received-data\\0004\\data\\updated\\plot_bounds_v4.gpkg", crs = 4326)

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

#### Export csv of attribute table ####

# polygons_csv <- st_drop_geometry(polygons)

# write.csv(polygons_csv, "C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\FOCAL.dispersal.kernel.project\\20240216_FOCAL_dispersal_kernel_polygons.csv")

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


## STEF MOC

#import data

MOC8boundary <- st_read ("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\1_received-data\\0006\\data\\RDS-2021-0061\\Data\\STEF_MOC_Plots_8_9_10_11_archive.gdb", layer = "MOC8_Boundary")

MOC9boundary <- st_read ("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\1_received-data\\0006\\data\\RDS-2021-0061\\Data\\STEF_MOC_Plots_8_9_10_11_archive.gdb", layer = "MOC9_Boundary")

MOC10boundary <- st_read ("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\1_received-data\\0006\\data\\RDS-2021-0061\\Data\\STEF_MOC_Plots_8_9_10_11_archive.gdb", layer = "MOC10_Boundary")

MOC11boundary <- st_read ("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\1_received-data\\0006\\data\\RDS-2021-0061\\Data\\STEF_MOC_Plots_8_9_10_11_archive.gdb", layer = "MOC11_Boundary_2007")

# Right now the MOC polygons are in UTM 10N. Transform to EPSG 4326.

MOC8boundary = st_transform(MOC8boundary, crs = 4326)
MOC9boundary = st_transform(MOC9boundary, crs = 4326)
MOC10boundary = st_transform(MOC10boundary, crs = 4326)
MOC11boundary = st_transform(MOC11boundary, crs = 4326)

# add OFO plot ID

MOC8boundary <- MOC8boundary %>%
  add_column(plot_id_ofo = "0069")

MOC9boundary <- MOC9boundary %>%
  add_column(plot_id_ofo = "0070")

MOC10boundary <- MOC10boundary %>%
  add_column(plot_id_ofo = "0071")

MOC11boundary <- MOC11boundary %>%
  add_column(plot_id_ofo = "0072")

# remove extra columns, we only want the polygon and the OFO plot ID

MOC8boundary <- MOC8boundary[c(3,4)]
MOC9boundary <- MOC9boundary[c(3,4)]
MOC10boundary <- MOC10boundary[c(3,4)]
MOC11boundary <- MOC11boundary[c(3,4)]

# Export polygon

st_write(MOC8boundary, "C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\2_standardized-data\\field-plot-boundaries\\0069.gpkg")

st_write(MOC9boundary, "C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\2_standardized-data\\field-plot-boundaries\\0070.gpkg")

st_write(MOC10boundary, "C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\2_standardized-data\\field-plot-boundaries\\0071.gpkg")

st_write(MOC11boundary, "C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\2_standardized-data\\field-plot-boundaries\\0072.gpkg")

#### STEF BERNAL ####

# import data

units_spatial <- st_read ("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\1_received-data\\0007\\data\\STEF_VDT_Derek_Young\\VDT_GIS\\VDT_unit_boundaries\\VDT_boundaries.shp")

# Right now the polygons are in UTM 10N. Transform to EPSG 4326.

units_spatial = st_transform(units_spatial, crs = 4326)

# add OFO plot ID

units_spatial <- units_spatial %>%
  add_column(plot_id_ofo = "0")

units_spatial$plot_id_ofo[units_spatial$UnitNo == '2'] <- '0073'
units_spatial$plot_id_ofo[units_spatial$UnitNo == '5'] <- '0074'
units_spatial$plot_id_ofo[units_spatial$UnitNo == '8'] <- '0075'
units_spatial$plot_id_ofo[units_spatial$UnitNo == '10'] <- '0076'
units_spatial$plot_id_ofo[units_spatial$UnitNo == '15'] <- '0077'
units_spatial$plot_id_ofo[units_spatial$UnitNo == '18'] <- '0078'
units_spatial$plot_id_ofo[units_spatial$UnitNo == '21'] <- '0079'
units_spatial$plot_id_ofo[units_spatial$UnitNo == '22'] <- '0080'

# remove extra columns, we only want the polygons and the OFO plot IDs

units_spatial <- units_spatial[c(5,4)]

# Export

for(i in 1:nrow(units_spatial)) {
  polygon_current <- units_spatial[i, ]
  plot_id_current = polygon_current$plot_id_ofo
  st_write(polygon_current, paste0("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\2_standardized-data\\field-plot-boundaries\\", plot_id_current, ".gpkg"))
}

#### San Jacinto Forest Geo ####

# x coordinate centroid is the 16s

# y coordinate centroid is the Fs

# therefore F16 is the centroid of the whole plot

# import data

SanJacintosubplots <- read.csv("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\1_received-data\\0008\\data\\SJFDP_Env_4ha_20230504.csv")

# remove extra columns that aren't subplot ID, lat, and long

SanJacintoCenter <- SanJacintosubplots[-c(2:5)]

SanJacintoCenter <- SanJacintoCenter[c(1:3)]

# remove extra rows that aren't F16

SanJacintoCenter <- SanJacintoCenter[-c(1:60, 62:121),]

SanJacintoCenter_sp <- st_as_sf(SanJacintoCenter, coords = c("Lat", "Long"), crs = 4326, remove=F)

# this is a square plot; the center point is the centroid. the edges are 100 m away from this center point in 4 directions (NOT cardinal directions, the plot is not oriented along a N-S-E-W axis). the corners are A11, A21, K11, and K21.

# make a data frame with the polygon corners

SanJacintoCorners <- SanJacintosubplots[-c(2:5)]

SanJacintoCorners <- SanJacintoCorners[c(1:3)]

SanJacintoCorners <- SanJacintoCorners[-c(2:10, 12:110, 112:120),]

SanJacintoCorners <- SanJacintoCorners %>% arrange(factor(Quad, levels = c('A11', 'K11', 'K21', 'A21')))

SanJacintoCornerClose <- SanJacintoCorners[-c(2:4),]

SanJacintoCorners <- rbind(SanJacintoCorners, SanJacintoCornerClose)

SanJacintoCornersPoints <- SanJacintoCorners [-(1)]

# create polygon in sf package

SanJacintoPolygon = st_polygon(
  list(
    cbind(
      SanJacintoCorners$Long[c(1,2,3,4,5)], 
      SanJacintoCorners$Lat[c(1,2,3,4,5)])
  )
)

SanJacintoPolygon = st_sfc(SanJacintoPolygon, crs=4326)

SanJacintoPolygon = st_as_sf (SanJacintoPolygon)

# add OFO plot ID

SanJacintoPolygon <- SanJacintoPolygon %>%
  add_column(plot_id_ofo = "0081")

# Export polygon

st_write(SanJacintoPolygon, "C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\2_standardized-data\\field-plot-boundaries\\0081.gpkg")

#### Lamping ####

# import plot data

Lampingplots <- st_read("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\1_received-data\\0009\\data\\Original\\Lamping_UAS_StemMap_boundarys\\doc.kml")

# st_crs(Lampingplots)
# plot KML is in 4326

Lampingplots <- Lampingplots [-(4),]

Lampingplots <- Lampingplots [-(2)]

# add ofo plot IDs

Lampingplots <- Lampingplots %>%
  add_column(plot_id_ofo = "")

Lampingplots$Name[Lampingplots$Name == 'OW'] <- 'SJER'

Lampingplots$Name[Lampingplots$Name == 'MC'] <- 'MC1'

Lampingplots$plot_id_ofo[Lampingplots$Name == 'BS2'] <- '0082'

Lampingplots$plot_id_ofo[Lampingplots$Name == 'MC1'] <- '0083'

Lampingplots$plot_id_ofo[Lampingplots$Name == 'SJER'] <- '0084'

Lampingplots$plot_id_ofo[Lampingplots$Name == 'TO1'] <- '0085'

Lampingplots$plot_id_ofo[Lampingplots$Name == 'TO2'] <- '0086'

Lampingplots$plot_id_ofo[Lampingplots$Name == 'UN3'] <- '0087'

Lampingplots <- Lampingplots [-(1)]

# Add 15m buffer-- first need to change to a coordinate system that's projected in meters with equal x and y distances: CONUS Albers Equal Area (EPSG: 5070). Then add the 15m buffer, then switch back to the WGS84 coordinate system

Lampingplots = st_transform(Lampingplots, crs = 5070)

Lampingplots <- st_buffer(Lampingplots, dist = 15)

Lampingplots = st_transform(Lampingplots, crs = 4326)

# Export

for(i in 1:nrow(Lampingplots)) {
  polygon_current <- Lampingplots[i, ]
  plot_id_current = polygon_current$plot_id_ofo
  st_write(polygon_current, paste0("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\2_standardized-data\\field-plot-boundaries\\", plot_id_current, ".gpkg"))
}

#### FOCAL early regen dispersal project ####

#import data

treedata <- read.csv("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\FOCAL_early_regen_dispersal\\20240110_FOCAL_early_regen_dispersal.csv", header = TRUE)

# we don't want all of these columns, just plot ID and spatial attributes

treedata = subset(treedata, select = c('ofo_plot_id','centerpoint_latitude', 'centerpoint_longitude') )

# and we don't need all of these rows, just one row per plot

treedata <- treedata [-(2:94),]
treedata <- treedata [-(3:21),]
treedata <- treedata [-(4:19),]

# convert to sf

treedata_sf <- st_as_sf(treedata, coords = c("centerpoint_longitude", "centerpoint_latitude"), crs = 4326)

# These are circular plots! Buffer circles by 30 meters-- this is the plot radius
# First project to a projected (meters) coordinate system with equal x and y distances
# CONUS Albers Equal Area (EPSG: 5070) covers the CONUS, but will need a different one for any future plots outside the CONUS

treedata_sf = st_transform(treedata_sf, crs = 5070)
treedata_circles <- st_buffer(treedata_sf, dist = c(50,30, 30), nQuadSegs = 10) # First plot (#112 is 50m radius), other 30m

# Then back to WGS84

treedata_circles <- st_transform(treedata_circles, crs = 4326)

# export plot polygons

for(i in 1:nrow(treedata_circles)) {
  polygon_current <- treedata_circles[i, ]
  plot_id_current = polygon_current$ofo_plot_id
  st_write(polygon_current, paste0("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\2_standardized-data\\field-plot-boundaries\\", plot_id_current, ".gpkg"))
}

#### Blodgett_Ryan ####

# import plot data

plots <- st_read("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\1_received-data\\0012\\data\\plot_boundaries_sf.geojson")

# st_crs(plots)
# plot crs is 26910

# add ofo plot IDs

plots <- plots %>%
  add_column(plot_id_ofo = "")

plots$plot_id_ofo[plots$plot_name == 'Control_240'] <- '0115'

plots$plot_id_ofo[plots$plot_name == 'Mech_350'] <- '0116'

plots$plot_id_ofo[plots$plot_name == 'MechBurn_380'] <- '0117'

plots$plot_id_ofo[plots$plot_name == 'Burn_400'] <- '0118'

# remove extraneous columns

plots <- plots [-(2)]

plots <- plots [-(1)]

# project to WGS 84

plots = st_transform(plots, crs = 4326)

# Export

for(i in 1:nrow(plots)) {
  polygon_current <- plots[i, ]
  plot_id_current = polygon_current$plot_id_ofo
  st_write(polygon_current, paste0("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\2_standardized-data\\field-plot-boundaries\\", plot_id_current, ".gpkg"))
}

#### TNC/IRI data ####

# import plot centers

plotcenters <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\allupdatedplotcentersWGS84.kml")

# we want to keep all the points that start with "L" and remove the points that start with "S"

plotcenters <- plotcenters %>% arrange(desc(Name))

plotcenters <- plotcenters [-(1:124),]

plotcenters <- plotcenters [-(2)]

# add ofo plot ID numbers

plotcenters$`ofo_plot_id` <- ""

plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L506'] <- '0088'
plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L511'] <- '0089'
plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L512'] <- '0090'
plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L515'] <- '0091'
plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L517'] <- '0092'
plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L525'] <- '0093'

plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L521'] <- '0099'
plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L505'] <- '0096'
plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L504'] <- '0095'
plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L536'] <- '0101'
plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L507'] <- '0097'
plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L502'] <- '0094'
plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L529'] <- '0100'
plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L508'] <- '0098'

plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L519'] <- '0102'

plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L522'] <- '0107'
plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L528'] <- '0104'
plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L524'] <- '0103'
plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L520'] <- '0106'
plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L535'] <- '0108'
plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L539'] <- '0109'
plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L534'] <- '0105'

plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L092'] <- '0110'

plotcenters$`ofo_plot_id`[plotcenters$`Name` == 'L527'] <- '0111'

plotcenters <- plotcenters [-(1)]

# These are circular plots! Buffer circles by 30.48 meters-- this is the plot radius
# First project to a projected (meters) coordinate system with equal x and y distances
# CONUS Albers Equal Area (EPSG: 5070) covers the CONUS, but will need a different one for any future plots outside the CONUS

plotcenters = st_transform(plotcenters, crs = 5070)
plotcenters_circles <- st_buffer(plotcenters, dist = 30.48, nQuadSegs = 10)

# Then back to WGS84

plotcenters_circles <- st_transform(plotcenters_circles, crs = 4326)

# export plot polygons

for(i in 1:nrow(plotcenters_circles)) {
  polygon_current <- plotcenters_circles[i, ]
  plot_id_current = polygon_current$ofo_plot_id
  st_write(polygon_current, paste0("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\2_standardized-data\\field-plot-boundaries\\", plot_id_current, ".gpkg"))
}

#### Johnston OSU ####

# There is lots of uncertainty in these coordinates and plots!!!!!!!!!!!

# Define plot bounds by buffering out the trees (10 m?), merging the resulting circles, and buffering back in the same amount. This will at least give ballparks to visualize with for now.

# Load tree point data

Johnston14_trees <- st_read ("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\OSU.stem_data\\stem_data_site14trees.gpkg")

Johnston15_trees <- st_read ("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\OSU.stem_data\\stem_data_site15trees.gpkg")

Johnston11_trees <- st_read ("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\OSU.stem_data\\stem_data_site11trees.gpkg")

Johnston10_trees <- st_read ("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\OSU.stem_data\\stem_data_site10trees.gpkg")

# add a new column with the right plot ID

Johnston14_trees <- Johnston14_trees %>%
  add_column(plot_id_ofo = "0053")

Johnston15_trees <- Johnston15_trees %>%
  add_column(plot_id_ofo = "0054")

Johnston11_trees <- Johnston11_trees %>%
  add_column(plot_id_ofo = "0055")

Johnston10_trees <- Johnston10_trees %>%
  add_column(plot_id_ofo = "0056")

# remove unneeded columns; only want ofo plot ID and geometry

Johnston14_trees = subset(Johnston14_trees, select = c("plot_id_ofo", "geom"))

Johnston15_trees = subset(Johnston15_trees, select = c("plot_id_ofo", "geom"))

Johnston11_trees = subset(Johnston11_trees, select = c("plot_id_ofo", "geom"))

Johnston10_trees = subset(Johnston10_trees, select = c("plot_id_ofo", "geom"))

# Buffer out the trees-- first need to change to a coordinate system that's projected in meters with equal x and y distances: CONUS Albers Equal Area (EPSG: 5070).

Johnston14_trees = st_transform(Johnston14_trees, crs = 5070)
Johnston14_trees_buffers <- st_buffer(Johnston14_trees, dist = 10)
# results in a multipolygon. buffered out to 30m and still a multipolygon. kept it at 10m for now. 
# Johnston14_trees_buffers <- st_buffer(Johnston14_trees, dist = 30)

Johnston15_trees = st_transform(Johnston15_trees, crs = 5070)
Johnston15_trees_buffers <- st_buffer(Johnston15_trees, dist = 10)

Johnston11_trees = st_transform(Johnston11_trees, crs = 5070)
Johnston11_trees_buffers <- st_buffer(Johnston11_trees, dist = 10)

Johnston10_trees = st_transform(Johnston10_trees, crs = 5070)
Johnston10_trees_buffers <- st_buffer(Johnston10_trees, dist = 10)

# Merge the resulting circles

Johnston14_trees_buffers_merged <- st_union(Johnston14_trees_buffers, by_feature = FALSE, is_coverage = FALSE)
plot (Johnston14_trees_buffers_merged) 

Johnston15_trees_buffers_merged <- st_union(Johnston15_trees_buffers, by_feature = FALSE, is_coverage = FALSE)
plot (Johnston15_trees_buffers_merged)

Johnston11_trees_buffers_merged <- st_union(Johnston11_trees_buffers, by_feature = FALSE, is_coverage = FALSE)
plot (Johnston11_trees_buffers_merged)

Johnston10_trees_buffers_merged <- st_union(Johnston10_trees_buffers, by_feature = FALSE, is_coverage = FALSE)
plot (Johnston10_trees_buffers_merged)

# Buffer back in 

Johnston14_trees_buffers_merged_unbuffered <- st_buffer(Johnston14_trees_buffers_merged, dist = -10)

Johnston15_trees_buffers_merged_unbuffered <- st_buffer(Johnston15_trees_buffers_merged, dist = -10)
plot(Johnston15_trees_buffers_merged_unbuffered)

Johnston11_trees_buffers_merged_unbuffered <- st_buffer(Johnston11_trees_buffers_merged, dist = -10)

Johnston10_trees_buffers_merged_unbuffered <- st_buffer(Johnston10_trees_buffers_merged, dist = -10)

# Then back to WGS84

Johnston14_trees_buffers_merged_unbuffered = st_transform(Johnston14_trees_buffers_merged_unbuffered, crs = 4326)

Johnston15_trees_buffers_merged_unbuffered = st_transform(Johnston15_trees_buffers_merged_unbuffered, crs = 4326)

Johnston11_trees_buffers_merged_unbuffered = st_transform(Johnston11_trees_buffers_merged_unbuffered, crs = 4326)

Johnston10_trees_buffers_merged_unbuffered = st_transform(Johnston10_trees_buffers_merged_unbuffered, crs = 4326)

# export plot polygons

st_write(Johnston14_trees_buffers_merged_unbuffered, "C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\2_standardized-data\\field-plot-boundaries\\0053.gpkg")

st_write(Johnston15_trees_buffers_merged_unbuffered, "C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\2_standardized-data\\field-plot-boundaries\\0054.gpkg")

st_write(Johnston11_trees_buffers_merged_unbuffered, "C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\2_standardized-data\\field-plot-boundaries\\0055.gpkg")

st_write(Johnston10_trees_buffers_merged_unbuffered, "C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\2_standardized-data\\field-plot-boundaries\\0056.gpkg")
