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

#### Need to export each plot polygon individually ####

for(i in 1:nrow(plotdata_circles)) {
  polygon_current <- plotdata_circles[i, ]
  plot_id_current = polygon_current$plot_id
  st_write(polygon_current, paste0("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\2_standardized-data\\field-plot-boundaries\\", plot_id_current, ".gpkg"))
}


