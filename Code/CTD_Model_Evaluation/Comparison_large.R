graphics.off()
rm(list = ls())

## load in some packages

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(chron)


load("pathway.Rdata") # data created in CTD*exploratory

setwd(pathway.to.linux)

# now set actually set the working directory within linux operating system
setwd("PhDCH1/Code/")
path.to.data <- "../Data/BODC/AllCDTData/"

## load in most recent master df



load("../Data/CTD_master_211108_v3_AllConverted.Rdata")


######################################
########################################
# First - WE REMOVE OUT-OF-DATE DATA

master.df <- subset(master.df, master.df$YEAR >= 1993 ) # 3016747
master.df <- subset(master.df, master.df$YEAR <= 2017) # 

## what are teh limits of our data?
# model: 1993-2020
# ctd: 1993 (now) to 2017

# this is because the model only has information after this time 


## now, let's load in the model   
model <- nc_open(file = "../Data/CMEMS/CMEMS-GLOBAL_001_030-so_thetao_bottomT-2000.nc")

print(model) # lets get that metadata

# let's take a look at the dimensions & time of our model

m.lon <- ncvar_get(model, "longitude") #dim = 598
m.lat <- ncvar_get(model, "latitude") # dim = 205
m.depth <- ncvar_get(model, "depth") # dim = 50
m.time <- ncvar_get(model, "time") # this is in Julian days - 31
m.t.units <- ncatt_get(model, "time", "units")

# let's convert the time so we know what we're actually looking at

library(chron)
m.t.time <- chron::month.day.year(jul = m.time/24, origin = c(month = 01, day = 01, year = 1950))

## so this data is daily data for the month of January 2020

## let's look at the master df & identify the locations that we need this for

jan.ctd <- subset(master.df, master.df$YEAR == 2000) ## I don't have
jan.ctd <- subset(jan.ctd, jan.ctd$MONTH == 1) # 48 thousand dips, marvelous

# how many locations have I got?
lat.long <- paste(jan.ctd$LAT, jan.ctd$LONG, sep = "_")
length(unique(lat.long)) # 35 different locations


max(master.df$YEAR)

## WORK THROUGH ONE VARIABLE AT A TIME

m.sal <- ncvar_get(model, "so")
m.temp <- ncvar_get(model, "thetao")
m.btemp <- ncvar_get(model, "bottomT")

dim(m.sal)
dim(m.temp)
dim(m.btemp) # exactly the same dimensions just without the 50 levels 

#print(model)
dim(m.sal) # 589 205  50  31 = long * lat * depth * days
# total number of rows in extractaed data is:
589*205*50*31 #187,154,750 and this is just for one month. 
589*205*50*31*12*(2008-1993) # 33,687,855,000 so this would be my TOTAL data size
# ok so a data frame with 33 BILLION rows might be a tad excessive. 
# I think I will just try and extra the data that I need. 

### either pick out the exact times first, or the exact locations
## let's whittle it down by times first

length(unique(jan.ctd$DATE)) # 10 days, great
days <- strsplit((unique(jan.ctd$DATE)), split = "1-")
days <- lapply(days, `[[`, 2)
days <- as.numeric(days)


ctd.date.normal <- month.day.year(jul = jan.ctd$JDATE, origin = c(month = 1, day = 1, year = -4713)) # convert to something useful
ctd.date.vector <- paste(ctd.date.normal$year, ctd.date.normal$month, ctd.date.normal$day, sep = "-")

jan.ctd$DAY <- ctd.date.normal$day

### we then want to extract each day at a time, and then get the lat/long that matches with the dates

#install.packages("data.table")
library(data.table)

all.model <- data.frame(SAL = NA, TEMP = NA, BOTTOM = NA, 
                                      LAT = NA, LONG = NA, DATE = NA, Z = NA)

for (x in days) {
  #x <- days[1]
  
  ctd.from.day <- subset(jan.ctd, jan.ctd$DAY == x)
  ### then extract the correct locations - need to make sure we extract the right pairs
  day.locations <- unique(paste(ctd.from.day$LAT, ctd.from.day$LONG, sep = "_"))
  day.locations <- strsplit(day.locations, split = "_")
  day.locations.df <- data.frame(LAT = lapply(day.locations, `[[`, 1), LONG = lapply(day.locations, `[[`, 2))
  colnames(day.locations.df) <- c("LAT", "LONG")
  
  for (i in 1:length(day.locations.df$LAT)) {
    lat <- as.numeric(day.locations.df$LAT[i])
    dt <- data.table(m.lat, val = m.lat)
    setattr(dt, "sorted", "m.lat")
    setkey(dt, m.lat)
    closest.lat <- dt[J(lat), roll = "nearest"]
    closest.ref.lat <- match(x = closest.lat$val, table = m.lat)
      # now to longitude 
    long <- as.numeric(day.locations.df$LONG[i])
    dt <- data.table(m.lon, val = m.lon)
    setattr(dt, "sorted", "m.lon")
    setkey(dt, m.lon)
    closest.lon <- dt[J(long), roll = "nearest"]
    closest.ref.lon <- match(x = closest.lon$val, table = m.lon)
    
    ###  now we extract all of the relevant model information 
    sal.dip<- m.sal[closest.ref.lon, closest.ref.lat,,x]
    temp.dip <- m.temp[closest.ref.lon, closest.ref.lat,,x]
    bottom.dip <- m.btemp[closest.ref.lon, closest.ref.lat,x]
    
    ## we save it down
    temp.model <- data.frame(SAL = sal.dip, TEMP = temp.dip, BOTTOM = bottom.dip, 
                             LAT = lat, LONG = long, DATE = x, Z = c(1:50))
    ## and then we store it 
    all.model <- rbind(all.model, temp.model)
    
   }
}


### can we just try plotting this data??

library(ggplot2)
library(gridExtra)
#install.packages("gridExtra")


## we want to plot temperature vs salinity - real & model!

mod.plot <- qplot(x = all.model$TEMP, y = all.model$SAL)
data.plot <- qplot(x = jan.ctd$TEMP_P, y = jan.ctd$SAL_A)

grid.arrange(mod.plot, data.plot, ncol = 2)

# NICE


####### LETS TRY THIS WITH A LARGE CTD FILE - PERHAPS ONE WITH >1 MONTH? 

model <- nc_open(file = "../Data/CMEMS/CMEMS-GLOBAL_001_030-so_thetao_bottomT-1997.nc")

m.lon <- ncvar_get(model, "longitude") #dim = 598
m.lat <- ncvar_get(model, "latitude") # dim = 205
m.depth <- ncvar_get(model, "depth") # dim = 50
m.time <- ncvar_get(model, "time") # this is in Julian days - 31
m.t.units <- ncatt_get(model, "time", "units")

# let's convert the time so we know what we're actually looking at

library(chron)
m.t.time <- chron::month.day.year(jul = m.time, origin = c(month = 01, day = 01, year = 1997))
# THIS IS A FULL YEAR OF DATA!

year.ctd <- subset(master.df, master.df$YEAR == 1997) 
# how many months 
length(unique(year.ctd$MONTH)) # just the one
length(unique(year.ctd$DATE)) # 10 days 

## as I have a year worth of data, instead of just looping by days, this time I am going to loop by months too!
## actuallly don't need to do that as most of my downloads are monthly tbh 
