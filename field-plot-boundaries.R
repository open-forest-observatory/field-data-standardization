# Author: Emily Marie Purvis
# Date: 10.2.2023
# Goal: Write code that creates polygons around plot centers and exports them to .gpkg files

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data")

#### Load libraries ####
library(sf)
library(tidyverse)

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

# plotdata$plot_id[plotdata$contributor_id == '1-A-145'] <- '{0001}'
# plotdata$plot_id[plotdata$contributor_id == '1-A-147'] <- '{0002}'
# plotdata$plot_id[plotdata$contributor_id == '1-A-150'] <- '{0003}'
# plotdata$plot_id[plotdata$contributor_id == '10-A-114'] <- '{0004}'
# plotdata$plot_id[plotdata$contributor_id == '10-A-115'] <- '{0005}'
# plotdata$plot_id[plotdata$contributor_id == '10-A-127'] <- '{0006}'
# plotdata$plot_id[plotdata$contributor_id == '11-A-104'] <- '{0007}'
# plotdata$plot_id[plotdata$contributor_id == '11-A-107'] <- '{0008}'
# plotdata$plot_id[plotdata$contributor_id == '11-A-109'] <- '{0009}'
# plotdata$plot_id[plotdata$contributor_id == '15-A-129'] <- '{0010}'

# plotdata$plot_id[plotdata$contributor_id == '15-A-135'] <- '{0011}'
# plotdata$plot_id[plotdata$contributor_id == '15-A-143'] <- '{0012}'
# plotdata$plot_id[plotdata$contributor_id == '18-A-56'] <- '{0013}'
# plotdata$plot_id[plotdata$contributor_id == '18-A-57'] <- '{0014}'
# plotdata$plot_id[plotdata$contributor_id == '18-A-62'] <- '{0015}'
# plotdata$plot_id[plotdata$contributor_id == '19-A-35'] <- '{0016}'
# plotdata$plot_id[plotdata$contributor_id == '19-A-37'] <- '{0017}'
# plotdata$plot_id[plotdata$contributor_id == '19-A-43'] <- '{0018}'
# plotdata$plot_id[plotdata$contributor_id == '19-A-48'] <- '{0019}'
# plotdata$plot_id[plotdata$contributor_id == '19-A-50'] <- '{0020}'

# plotdata$plot_id[plotdata$contributor_id == '2-A-151'] <- '{0021}'
# plotdata$plot_id[plotdata$contributor_id == '2-A-152'] <- '{0022}'
# plotdata$plot_id[plotdata$contributor_id == '2-A-157'] <- '{0023}'
# plotdata$plot_id[plotdata$contributor_id == '26-A-74'] <- '{0024}'
# plotdata$plot_id[plotdata$contributor_id == '26-A-71'] <- '{0025}'
# plotdata$plot_id[plotdata$contributor_id == '26-A-72'] <- '{0026}'
# plotdata$plot_id[plotdata$contributor_id == '26-A-87'] <- '{0027}'
# plotdata$plot_id[plotdata$contributor_id == '26-B-73'] <- '{0028}'
# plotdata$plot_id[plotdata$contributor_id == '3-A-166'] <- '{0029}'
# plotdata$plot_id[plotdata$contributor_id == '3-A-169'] <- '{0030}'

# plotdata$plot_id[plotdata$contributor_id == '3-A-170'] <- '{0031}'
# plotdata$plot_id[plotdata$contributor_id == '3-A-182'] <- '{0032}'
# plotdata$plot_id[plotdata$contributor_id == '3-A-305'] <- '{0033}'
# plotdata$plot_id[plotdata$contributor_id == '3-B-167'] <- '{0034}'
# plotdata$plot_id[plotdata$contributor_id == '4-A-180'] <- '{0035}'
# plotdata$plot_id[plotdata$contributor_id == '4-A-194'] <- '{0036}'
# plotdata$plot_id[plotdata$contributor_id == '4-A-195'] <- '{0037}'
# plotdata$plot_id[plotdata$contributor_id == '6-A-208'] <- '{0038}'
# plotdata$plot_id[plotdata$contributor_id == '6-A-181'] <- '{0039}'
# plotdata$plot_id[plotdata$contributor_id == '6-A-184'] <- '{0040}'

# plotdata$plot_id[plotdata$contributor_id == '6-A-189'] <- '{0041}'
# plotdata$plot_id[plotdata$contributor_id == '6-A-198'] <- '{0042}'
# plotdata$plot_id[plotdata$contributor_id == '6-A-203'] <- '{0043}'
# plotdata$plot_id[plotdata$contributor_id == '6-A-207'] <- '{0044}'
# plotdata$plot_id[plotdata$contributor_id == '7-A-304'] <- '{0045}'
# plotdata$plot_id[plotdata$contributor_id == '7-A-94'] <- '{0046}'
# plotdata$plot_id[plotdata$contributor_id == '7-A-96'] <- '{0047}'
# plotdata$plot_id[plotdata$contributor_id == '7-D-97'] <- '{0048}'
# plotdata$plot_id[plotdata$contributor_id == '8-A-301'] <- '{0049}'
# plotdata$plot_id[plotdata$contributor_id == '8-A-302'] <- '{0050}'

# plotdata$plot_id[plotdata$contributor_id == '8-B-303'] <- '{0051}'


# a way faster way of doing this

plotdata$plot_id <- 1:nrow(plotdata)

# future datasets can use something like plotdata$plot_id <- (1:nrow(plotdata) + 51)

# remove contributor id column

plotdata = subset(plotdata, select = c('plot_id','lon', 'lat') )

# convert to sf

plotdata_sf <- st_as_sf(plotdata, coords = c("lon", "lat"), crs = 4326)

# These are circular plots! Buffer circles by 100 feet (30.48 meters)-- this is the plot radius
#   First project to a projected (meters) coordinate system with equal x and y distances
#    CONUS Albers Equal Area (EPSG: 5070) covers the CONUS, but will need a different one for any future plots outside the CONUS
plotdata_sf = st_transform(plotdata_sf, crs = 5070)
plotdata_circles <- st_buffer(plotdata_sf, dist = 30.48, nQuadSegs = 10)
# Then back to WGS84
plotdata_circles <- st_transform(plotdata_circles, crs = 4326)


#### Need to export each plot polygon individually ####

# again desparately trying to write for loops & giving up

for(i in 1:nrow(plotdata_circles)) {
  polygon_current <- plotdata_circles[i, ]
  plot_id_current = polygon_current$plot_id
  st_write(polygon_focal, paste0("ofo-field-data\\2_standardized-data\\field-plot-boundaries\\", plot_id_current, ".gpkg"))
}


# create individual sf files for each plot

why <- plotdata_circles[1,]
"{0002}" <- plotdata_circles[2,]
"{0003}" <- plotdata_circles[3,]
"{0004}" <- plotdata_circles[4,]
"{0005}" <- plotdata_circles[5,]
"{0006}" <- plotdata_circles[6,]
"{0007}" <- plotdata_circles[7,]
"{0008}" <- plotdata_circles[8,]
"{0009}" <- plotdata_circles[9,]
"{0010}" <- plotdata_circles[10,]

"{0011}" <- plotdata_circles[11,]
"{0012}" <- plotdata_circles[12,]
"{0013}" <- plotdata_circles[13,]
"{0014}" <- plotdata_circles[14,]
"{0015}" <- plotdata_circles[15,]
"{0016}" <- plotdata_circles[16,]
"{0017}" <- plotdata_circles[17,]
"{0018}" <- plotdata_circles[18,]
"{0019}" <- plotdata_circles[19,]
"{0020}" <- plotdata_circles[20,]

"{0021}" <- plotdata_circles[21,]
"{0022}" <- plotdata_circles[22,]
"{0023}" <- plotdata_circles[23,]
"{0024}" <- plotdata_circles[24,]
"{0025}" <- plotdata_circles[25,]
"{0026}" <- plotdata_circles[26,]
"{0027}" <- plotdata_circles[27,]
"{0028}" <- plotdata_circles[28,]
"{0029}" <- plotdata_circles[29,]
"{0030}" <- plotdata_circles[30,]

"{0031}" <- plotdata_circles[31,]
"{0032}" <- plotdata_circles[32,]
"{0033}" <- plotdata_circles[33,]
"{0034}" <- plotdata_circles[34,]
"{0035}" <- plotdata_circles[35,]
"{0036}" <- plotdata_circles[36,]
"{0037}" <- plotdata_circles[37,]
"{0038}" <- plotdata_circles[38,]
"{0039}" <- plotdata_circles[39,]
"{0040}" <- plotdata_circles[40,]

"{0041}" <- plotdata_circles[41,]
"{0042}" <- plotdata_circles[42,]
"{0043}" <- plotdata_circles[43,]
"{0044}" <- plotdata_circles[44,]
"{0045}" <- plotdata_circles[45,]
"{0046}" <- plotdata_circles[46,]
"{0047}" <- plotdata_circles[47,]
"{0048}" <- plotdata_circles[48,]
"{0049}" <- plotdata_circles[49,]
"{0050}" <- plotdata_circles[50,]

"{0051}" <- plotdata_circles[51,]

# export

# at first I was getting an error message that said I couldn't export a character as a file...figured out that st_write didn't like my sf object names. that's why I changed the first one to "why" and kept trying to export

st_write(why, data("ofo-field-data\\2_standardized-data\\field-plot-boundaries\\{0001}.gpkg"))

# now I've gotten past that error but keep getting new error messages: "In data("ofo-field-data\\2_standardized-data\\field-plot-boundaries\\{0001}.gpkg") :data set ‘ofo-field-data\2_standardized-data\field-plot-boundaries\{0001}.gpkg’ not found" AND "In CPL_write_ogr(obj, dsn, layer, driver, as.character(dataset_options),  :GDAL Error 4: sqlite3_open(ofo-field-data\2_standardized-data\field-plot-boundaries\{0001}.gpkg) failed: unable to open database file


