#### set up
graphics.off()
rm(list = ls())

## load in some packages

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(chron)
library(data.table)

load("pathway.Rdata") # data created in CTD*exploratory

setwd(pathway.to.linux)

# now set actually set the working directory within linux operating system
setwd("PhDCH1/Code/")
path.to.data <- "../Data/BODC/AllCDTData/"

## load in most recent master df

load("../Data/CTD_master_211108_v3_AllConverted.Rdata")

## add in day data

ctd.date.normal <- month.day.year(jul = master.df$JDATE, 
                                  origin = c(month = 1, day = 1, year = -4713)) # convert to something useful
ctd.date.vector <- paste(ctd.date.normal$year, ctd.date.normal$month, ctd.date.normal$day, sep = "-")
master.df$DAY <- ctd.date.normal$day

# step one (elsewhere): create and save a list of the names of all of the model output files
# store them all in one directory

to.models<- "D:/CMEMS_001_030_MONTHLY/"

model.list <- list.files(path = to.models)

# step three: read in the most recent master data.frame & trim the dates so everythiNg matches

## set up master storage data frame

all.model.data <- data.frame(  SAL = NA, TEMP = NA, BOTTOM = NA, 
                               modLAT = NA , ctdLAT = NA,
                               modLONG = NA, ctdLON = NA,
                               DATE = NA, 
                               modDEPTH = NA, 
                               UNIQ = NA)

### read in first model data

## create my second master data.frame 

for (i in 1:length(model.list)) {
  
  print(paste("Starting model number ", i, "of ", length(model.list), sep = ""))
  model <- nc_open(file = paste(to.models, model.list[i], sep = ""))
  
  # look at what is inside 
  m.lon <- ncvar_get(model, "longitude") #dim = 598
  m.lat <- ncvar_get(model, "latitude") # dim = 205
  m.depth <- ncvar_get(model, "depth") # dim = 50
  m.time <- ncvar_get(model, "time") # this may vary depending on how many months in a year
  m.t.units <- ncatt_get(model, "time", "units")
  # let's convert the time so we know what we're actually looking at

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
  
  year <- unique(m.t.time$year)       
  month <- unique(m.t.time$month)
  
  #now subset the master.df for the month that we want 
  temp.ctd <- subset(master.df, master.df$YEAR == year) 
  temp.ctd <- subset(temp.ctd, temp.ctd$MONTH == month) 
  
  
  # extract our three variables 
  
  m.sal <- ncvar_get(model, "so")
  m.temp <- ncvar_get(model, "thetao")
  m.btemp <- ncvar_get(model, "bottomT")
  
  
  
  # we are only looping through the relevant days in the month (for sanity/ size sake)
  
  days <- unique(temp.ctd$DAY)
  
  one.month.data <- data.frame(  SAL = NA, TEMP = NA, BOTTOM = NA, 
                                 modLAT = NA , ctdLAT = NA,
                                 modLONG = NA, ctdLON = NA,
                                 DATE = NA, 
                                 modDEPTH = NA, 
                                 UNIQ = NA)
  


  counter <- 0
  for (x in days) {
    counter <- counter + 1
    print(paste("Day ", counter, " of ", length(days)))
    #x <- days[1]
    
    ctd.from.day <- subset(temp.ctd, temp.ctd$DAY == x)
    ### then extract the correct locations - need to make sure we extract the right pairs
    day.locations <- unique(paste(ctd.from.day$LAT, ctd.from.day$LONG, sep = "_"))
    day.locations <- strsplit(day.locations, split = "_")
    day.locations.df <- data.frame(LAT = lapply(day.locations, `[[`, 1), LONG = lapply(day.locations, `[[`, 2))
    colnames(day.locations.df) <- c("LAT", "LONG")
    
    for (j in 1:length(day.locations.df$LAT)) {
      
      lat <- as.numeric(day.locations.df$LAT[j]) # this lat is the data lat
      dt <- data.table(m.lat, val = m.lat)
      setattr(dt, "sorted", "m.lat")
      setkey(dt, m.lat)
      closest.lat <- dt[J(lat), roll = "nearest"] # closest lat is model lat 
      # we then find where the matching lat is (for indexing purposes)
      closest.ref.lat <- match(x = closest.lat$val, table = m.lat)
      # now to longitude 
      long <- as.numeric(day.locations.df$LONG[j])
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
                               modLAT = closest.lat$val , ctdLAT = lat,
                               modLONG = closest.lon$val, ctdLON = long,
                               DATE = ctd.from.day$DATE[1], 
                               modDEPTH = m.depth, 
                               UNIQ = paste(lat, long, ctd.from.day$DATE[1], sep = ""))
     
      ## and then we store it 
      one.month.data <- rbind(one.month.data, temp.model)
     
    }
    #name.to.save.monthly <- paste("../Data/CMEMS_Monthly", year, month, ".Rdata", sep = "_")
    #save(one.month.data, file = name.to.save.monthly)
  }

  
  ### now we can also try and create a master.df 
  
  all.model.data <- rbind(all.model.data, one.month.data)
  
  #name.to.save.all <- paste("../Data/CMEMS_All_File_", i, "_of", length(model.list), ".Rdata", sep = "")
  
  #save(all.model.data, file = name.to.save.all)
  nc_close(model)
  
}

name.to.save.all <- paste("../Data/CMEMS_All_File_", i, "_of", length(model.list), ".Rdata", sep = "")

save(all.model.data, file = name.to.save.all)

