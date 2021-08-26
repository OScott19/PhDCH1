graphics.off()
rm(list = ls())
load("pathway.Rdata") # data created in CTD*exploratory

setwd(pathway.to.linux)

# now set actually set the working directory within linux operating system
setwd("PhDCH1/Code/")

######################
# and load in my DF of all the relevant CTD files

load("../Data/BODC/MoreRelevantFiles.Rdata") # generated in CTD*exploratory
rm(all.df) # not necessary


######################
### load in necessary packages

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(chron) # converts julian dates (important!)


######################
# prime the counter and the dataframes
path.to.data <- "../Data/BODC/AllCDTData/"




##### now we have all of the relevant data extracted (as afar as I know)
# check what I've actually got! 
# do I have all the relevant data for every thing? 

file.vec <- NA
sal.vec <- NA
con.vec <- NA

for (i in 1:7) {
  nametoload <- paste("../Data/LongMasterDF_", i, (".Rdata"), sep = "")
  load(nametoload)
  file.vec <- c(file.vec, master.df$FILE)
  sal.vec <- c(sal.vec, master.df$SALINITY)
  con.vec <- c(con.vec, master.df$CONDUCTIVITY)
}

length(unique(con.vec)) # no conductivity data was captured, so this is all NA

length(unique(sal.vec)) # quite a lot of different salinties, which is good
# howwever a lot of errors did pop up with the salinities, so I might 
# check which files had zero salinity


fs <- data.frame(FILE = file.vec, SAL = sal.vec)

# so, which files didn't have any saility data?


fs.ns <- fs[is.na(fs$SAL),] # tbh this seems like most of them which isn't ideal


fs.ns.u <- unique(fs.ns$FILE) # 2754 files don't have saility data
# as PS001 - so clearly something else going on 


# let's open up the first one and see what salinity is stored as

library(ncdf4) # package for netcdf manipulation
path.to.data <- "../Data/BODC/AllCDTData/"

w.data <- nc_open(file = paste(path.to.data, fs.ns.u[2], sep = ""))
print(w.data)


# in my other exploratory we created the master df 
# now - let's check if we actually have all of the info in every column
# also looking to convert the PRESSURE info to depth
# need to find the right file for this - so open one using "depth"
# and check the metadata to find which algo they used



load("../Data/CTD_master_withTemp.Rdata")

length(unique(master.df$FILE[is.na(master.df$MAXDEPTH)])) 
# half of the records don't include bathymetric depth
# that's weird
# let's investigate

no.bath.depth <- unique(master.df$FILE[is.na(master.df$MAXDEPTH)])


library(ncdf4)

path.to.data <- "../Data/BODC/AllCDTData/"
w.data <- nc_open(file = paste(path.to.data, no.bath.depth[2], sep = ""))
# where is me bathymetric depth?

print(w.data)

# bathymetric data is stored in SDN_BOT_DEPTH - which is what we have!

check <- master.df[master.df$FILE == no.bath.depth[100],] # so, all NAs or just one?
# all NAs!
# so, why didn't this work? 

ncvar_get(w.data, "SDN_BOT_DEPTH")

check$MAXDEPTH <- ncvar_get(w.data, "SDN_BOT_DEPTH")
w.data$var$SDN_BOT_DEPTH$

print(w.data)


# conclusion - over half of them don't 


# let's just double check with the succesful ones

has.bath <- unique(master.df$FILE[is.na(master.df$MAXDEPTH) == F])

w.data <- nc_open(file = paste(path.to.data, has.bath[22], sep = ""))
# where is me bathymetric depth?
ncvar_get(w.data, "SDN_BOT_DEPTH")

print(w.data)

# bathymetric data is stored in SDN_BOT_DEPTH - which is what we have!

check <- master.df[master.df$FILE == no.bath.depth[100],] # so, all NAs or just one?

# all rows start with an NA, so let's remove
master.df <- master.df[-1,]

# so it appears someetimes we just don't have bathymetric data. 
# check other entries 
# not going to check one with a known mishap though
length(master.df$I[is.na(master.df$I)]) # None!
length(master.df$DATE[is.na(master.df$DATE)])
length(master.df$JULIANINFO[is.na(master.df$JULIANINFO)])
length(master.df$TEMPERATURE[is.na(master.df$TEMPERATURE)]) # quite a few
length(master.df$TEMPERATURE_UNIT[is.na(master.df$TEMPERATURE_UNIT)]) # more than before - weird!
length(master.df$LAT[is.na(master.df$LAT)]) # back to None! 
length(master.df$LONG[is.na(master.df$LONG)]) # back to None phew 
length(master.df$CRUISEID[is.na(master.df$CRUISEID)]) # back to None phew 
length(master.df$FILE[is.na(master.df$FILE)]) # back to None phew 

### so it turns out that we have quite a lot more missing temperature data than I thought
# but everything else is ok

## Ok - on to standardisation
length(unique(master.df$TEMPERATURE_UNIT)) # two different ways of measuring temp - degC and NA

length(unique(master.df$JULIANINFO))
unique(master.df$JULIANINFO) # which is days since -4713-01-01

# all stored in the same way

# let's convert all of the dates then
ctd.date.normal <- month.day.year(jul = master.df$DATE, origin = c(month = 1, day = 1, year = -4713)) # convert to something useful
ctd.date.vector <- paste(ctd.date.normal$year, ctd.date.normal$month, ctd.date.normal$day, sep = "-")
# and add this into the master

master.df$JDATE <- master.df$DATE
master.df$DATE <- ctd.date.vector
length(unique(master.df$DATE)) # 989 unique days of data
length(unique(ctd.date.normal$year)) # 33 years of data!

# right - what else needs to be normalised?
# Pressure needs to turn to depth!

got.depth <- master.df$FILE[is.na(master.df$DEPTH) == F]

# now lets open one of these ones

w.data <- nc_open(file = paste(path.to.data, got.depth[2], sep= ""))

print(w.data)

# description: 
# Depth (spatial coordinate) relative to water surface in the 
#w ater body by profiling pressure sensor and conversion to seawater 
# depth using UNESCO algorithm

# WHICH UNESCO algorithm??
# this one, I think: http://www.code10.info/index.php?option=com_content&view=article&id=67:calculating-the-depth-from-pressure&catid=54:cat_coding_algorithms_seawater&Itemid=79
# find R package that can handle

install.packages("marelac")

pressures <- data.frame(PRES = master.df$PRESS, UNIT = master.df$PRESSTXT, SAL = master.df$SALINITY, 
                        TEMP = master.df$TEMPERATURE, LAT = master.df$LAT)

one.pressure <- pressures[1,]
one.depth <- marelac::sw_depth(P = (one.pressure$PRES), lat = one.pressure$LAT)
# gives us a depth of -7. 
# however if we make the pressure larger, then the dept goes positive, so not sure this is good
# this doesn't really make sense, and also makes huge assumptions 
# water of salinity 35, and temperature 0 dg C - both wrong!


# let's try a different package

library(oce)

anothyer.depth <- oce::swDepth(pressure = (one.pressure$PRES), latitude = one.pressure$LAT, eos = "unesco")
# this gives us a more sensible depth of 3m (not a negative depth) - 
