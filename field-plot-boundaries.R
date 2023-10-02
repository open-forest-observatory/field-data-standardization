# Author: Emily Marie Purvis
# Date: 10.2.2023
# Goal: Write code that creates polygons around plot centers and exports them to .gpkg files

#### Set working directory ####
setwd("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data")

#### Load libraries ####
library(sf)
library(dplyr)

#### Load data ####

plotdata = read.csv("C:\\Users\\emily\\Box\\FOCAL\\ofo-field-data\\1_received-data\\{0001}\\data\\data\\vp_ctlsierra2022_v1_plots.csv", header = TRUE)
