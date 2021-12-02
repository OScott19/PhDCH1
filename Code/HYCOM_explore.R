#### set up

graphics.off()
rm(list = ls())

## load in some packages

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis


load("pathway.Rdata") # data created in CTD*exploratory

setwd(pathway.to.linux)

# now set actually set the working directory within linux operating system
setwd("PhDCH1/Code/")
path.to.data <- "../Data/BODC/AllCDTData/"

model <- nc_open("../Data/HYCOM/data_1994.nc4")

m.time <- ncvar_get(model, "time")
m.t.units <- ncatt_get(model, "time", "units")


m.t.units <- strsplit(m.t.units$value, split = " ")
if (m.t.units[[1]][1] == "days") {
  div <- 1
}
if (m.t.units[[1]][1] == "hours") {
  div <- 24
}
m.t.units <- strsplit(m.t.units[[1]][3], split = "-")
m.t.time <- chron::month.day.year(jul = m.time/div, 
                                  origin = c(month = as.numeric(m.t.units[[1]][2]), 
                                             day = as.numeric(m.t.units[[1]][3]), 
                                             year = as.numeric(m.t.units[[1]][1])))


## how many depths is this? 
m.depth <- ncvar_get(model, "depth") # dim = 40
m.lon <- ncvar_get(model, "lon") #dim = 626
m.lat <- ncvar_get(model, "lat") # dim = 426

### this is just a single day of data
# need more!

### exploring the inputs of the wget script
#nc_close(model)

model <- nc_open("GLBv0.08_expt_53.X_2015-01-01T00Z.nc")

m.time <- ncvar_get(model, "time")
m.t.units <- ncatt_get(model, "time", "units")


m.t.units <- strsplit(m.t.units$value, split = " ")
if (m.t.units[[1]][1] == "days") {
  div <- 1
}
if (m.t.units[[1]][1] == "hours") {
  div <- 24
}
m.t.units <- strsplit(m.t.units[[1]][3], split = "-")
m.t.time <- chron::month.day.year(jul = m.time/div, 
                                  origin = c(month = as.numeric(m.t.units[[1]][2]), 
                                             day = as.numeric(m.t.units[[1]][3]), 
                                             year = as.numeric(m.t.units[[1]][1])))


## how many depths is this? 
m.depth <- ncvar_get(model, "depth") # dim = 40
m.lon <- ncvar_get(model, "lon") #dim = 626
m.lat <- ncvar_get(model, "lat") # dim = 426

### this is just a single day of data
##### DOWNLOAD THREE

# have a look at the openDAP stuff


model <- nc_open("../Code/hycom_GLBv0.08_539_2015010112_t000_d5stride_varsubset.nc4")

m.time <- ncvar_get(model, "time")
m.t.units <- ncatt_get(model, "time", "units")


m.t.units <- strsplit(m.t.units$value, split = " ")
if (m.t.units[[1]][1] == "days") {
  div <- 1
}
if (m.t.units[[1]][1] == "hours") {
  div <- 24
}
m.t.units <- strsplit(m.t.units[[1]][3], split = "-")
m.t.time <- chron::month.day.year(jul = m.time/div, 
                                  origin = c(month = as.numeric(m.t.units[[1]][2]), 
                                             day = as.numeric(m.t.units[[1]][3]), 
                                             year = as.numeric(m.t.units[[1]][1])))


## how many depths is this? 
m.depth <- ncvar_get(model, "depth") # dim = 40
m.lon <- ncvar_get(model, "lon") #dim = 626
m.lat <- ncvar_get(model, "lat") # dim = 426

### idk what is going on here tbh


##################
#########################
# have a look at the openDAP stuff


model <- nc_open("../Data/data_1994.nc4") # ok I think this is a month of data

m.time <- ncvar_get(model, "time")
m.t.units <- ncatt_get(model, "time", "units")


m.t.units <- strsplit(m.t.units$value, split = " ")
if (m.t.units[[1]][1] == "days") {
  div <- 1
}
if (m.t.units[[1]][1] == "hours") {
  div <- 24
}
m.t.units <- strsplit(m.t.units[[1]][3], split = "-")
m.t.time <- chron::month.day.year(jul = m.time/div, 
                                  origin = c(month = as.numeric(m.t.units[[1]][2]), 
                                             day = as.numeric(m.t.units[[1]][3]), 
                                             year = as.numeric(m.t.units[[1]][1])))


## how many depths is this? 
m.depth <- ncvar_get(model, "depth") # still only two depths though 
m.lon <- ncvar_get(model, "lon") #dim = 613
m.lat <- ncvar_get(model, "lat") # dim = 426

### ok we're up to a month but still don't have the depth properly
# this mdoel has 40 depth layers and we want them ALL
# back to manal download 