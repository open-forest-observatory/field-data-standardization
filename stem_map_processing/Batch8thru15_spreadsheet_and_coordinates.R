# Author: Emily Marie Purvis
# Date: 12.5.2023
# Goal: combine Batch 8-15 GPS coordinates into KML and combine Batch 1-7 spreadsheets 

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(stringr)

#### Combine updated coordinates for all the batches into one KML ####

# Load batches 1-7 data

plots1thru7WGS84 <- read_sf ("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.IRI.Compiled.Data\\allupdatedplotcentersWGS84.kml")
