getwd()

setwd("../../../Code/") 

rm(list=ls()) # clean the workspace
graphics.off()

# going to load in generic plotting packages and hope this helps

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting


# we have two sets of data, let's load one in at a time



drifter <- nc_open(file = "../Data/Drifters/interpolated_gld.20210520_024116/interpolated_gld.20210520_024116")

# not working - I think becuase of the naming system
# re-named so that there is no .numbers, and there is instead .nc

drifter <- nc_open(file = "../Data/Drifters/interpolated_gld.20210520_024116/interpolated_drifter_data1.nc")

drifter <- read.oce("../Data/Drifters/interpolated_gld.20210520_024116/interpolated_drifter_data1.nc")
