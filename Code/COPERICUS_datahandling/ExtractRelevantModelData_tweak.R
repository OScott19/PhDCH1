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

all.model.list <- list.files(path = to.models)

## right, now we only want to read in models that correspond with months that we have ctd data for (otherwise a waste of time!)
## so, we extract the date part of each model name

ch <- as.numeric(nchar(all.model.list[1]))
model.dates <- as.numeric(substring(all.model.list, ch-8, ch-3)) ## this is so much easier than string.split 
#325 of these - as expected, this is the number of files that we have

## now create a vector of all of the ctd data month & year combo
## but need to re-format the month data first

master.df$MONTH <- as.numeric(master.df$MONTH)
master.df$MONTH[master.df$MONTH <10] <- paste(as.character("0"), master.df$MONTH[master.df$MONTH < 10], sep = "")
#master.df$MONTH[master.df$MONTH <10] <- substring(master.df$MONTH[master.df$MONTH <10], 4, 4)

ctd.dates <- paste(as.character(master.df$YEAR), master.df$MONTH, sep = "")
ctd.dates <- unique(ctd.dates) #93 months of data
# now check overlap 
models.to.read <- subset(model.dates, model.dates %in% ctd.dates) ## we now have 92 overlapping files, thank goodness

max(as.numeric(ctd.dates)) # 201712
min(as.numeric(ctd.dates)) # 199112

max(model.dates) #201912
min(model.dates) #199301

## check overlap
max(models.to.read) # 201712
min(models.to.read)#199301

## now, select only the files that overlap 

models.to.run <- data.frame(PATH = all.model.list, DATE = model.dates)
models.to.run <- subset(models.to.run, models.to.run$DATE %in% ctd.dates)

model.list <- models.to.run$PATH

save(model.list, file = "../Data/CMEMS_models_tocompare.Rdata")

## set up master storage data frame

all.model.data <- data.frame(SAL.m1 = NA,
                              SAL.m2 = NA,
                              TEMP.m1 = NA, 
                              TEMP.m2 = NA, 
                              BOTTOM.m1 = NA, 
                              BOTTOM.m2 = NA, 
                              modLAT.m1 = NA,
                              modLAT.m2 = NA,
                              ctdLAT = NA,
                              modlon.m1 = NA,
                              modlon.m2 = NA,
                              ctdLON = NA,
                              DATE = NA, 
                              modDEPTH = NA, 
                              UNIQ = NA)
### read in first model data

## create my second master data.frame 
#i <- 1
for (i in 1:length(model.list)) {

  #for (i in 1:1) {
  print(paste("Starting model number ", i, " of ", length(model.list), sep = ""))
  model <- nc_open(file = paste(to.models, model.list[i], sep = ""))
  
  # look at what is inside 
  m.lon <- ncvar_get(model, "longitude") #dim = 598
  m.lat <- ncvar_get(model, "latitude") # dim = 205
  m.depth <- ncvar_get(model, "depth") # dim = 50
  m.time <- ncvar_get(model, "time") # this may vary depending on how many months in a year
  m.t.units <- ncatt_get(model, "time", "units")
  # let's convert the time so we know what we're actually looking at
  
  
  ## make data tables of the lat & lon as we will use these repeatedly later
  
  dt.lat <- data.table(m.lat, val = m.lat)
  setattr(dt.lat, "sorted", "m.lat")

  dt.lon <- data.table(m.lon, val = m.lon)
  setattr(dt.lon, "sorted", "m.lon")

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
  if (unique(m.t.time$month) < 10) {
    month <- paste("0", unique(m.t.time$month), sep = "")
  } else {
    month <- unique(m.t.time$month)}
  
  #now subset the master.df for the month that we want 
  temp.ctd <- subset(master.df, master.df$YEAR == year) 
  temp.ctd <- subset(temp.ctd, temp.ctd$MONTH == month) 
  
  
  # extract our three variables 
  
  m.sal <- ncvar_get(model, "so")
  m.temp <- ncvar_get(model, "thetao")
  m.btemp <- ncvar_get(model, "bottomT")
  
  
  # we are only looping through the relevant days in the month (for sanity/ size sake)
  
  days <- unique(temp.ctd$DAY)
  
  one.month.data <- data.frame(SAL.m1 = NA,
                          SAL.m2 = NA,
                          TEMP.m1 = NA, 
                          TEMP.m2 = NA, 
                          BOTTOM.m1 = NA, 
                          BOTTOM.m2 = NA, 
                          modLAT.m1 = NA,
                          modLAT.m2 = NA,
                          ctdLAT = NA,
                          modlon.m1 = NA,
                          modlon.m2 = NA,
                          ctdLON = NA,
                          DATE = NA, 
                          modDEPTH = NA, 
                          UNIQ = NA)
  


  counter <- 0
  #x <- days[1]
  for (x in days) {
    counter <- counter + 1
    print(paste("Day ", counter, " of ", length(days)))
    #x <- days[1]
    
    ctd.from.day <- subset(temp.ctd, temp.ctd$DAY == x)
    ### then extract the correct locations - need to make sure we extract the right pairs
    ctd.from.day$latlon <- paste(ctd.from.day$LAT, ctd.from.day$LONG, sep = "_")
    unique.ctd.location <- ctd.from.day[!duplicated(ctd.from.day$latlon),]
    day.locations.df <- data.frame(LAT = unique.ctd.location$LAT, 
                                   LON = unique.ctd.location$LONG)
    colnames(day.locations.df) <- c("LAT", "lon")
    #j <- 1
    for (j in 1:length(day.locations.df$LAT)) {
      print(paste("Location", j, " of ", length(day.locations.df$LAT)))
      
      day.lat <- as.numeric(day.locations.df$LAT[j]) # this lat is the ctd lat
      setkey(dt.lat, m.lat)
      closest.lat <- dt.lat[J(day.lat), roll = "nearest"] # closest lat is model lat 
      # we then find where the matching lat is in the table of latitudes (for indexing purposes)
      closest.ref.lat.m1 <- match(x = closest.lat$val, table = m.lat)
      
      ## now how about we do it manually?
      
      lat.diff <- data.frame(lat = m.lat, diff = NA, closest = NA)
      lat.diff$diff <- abs(lat.diff$lat - day.lat)
      closest.ref.lat.m2 <- match(x = min(lat.diff$diff, na.rm = T), table = lat.diff$diff)
       
 
      # now to longitude 
      day.lon <- as.numeric(day.locations.df$lon[j])
      setkey(dt.lon, m.lon)
      closest.lon <- dt.lon[J(day.lon), roll = "nearest"]
      closest.ref.lon.m1 <- match(x = closest.lon$val, table = m.lon)
      
      ## and now manually?
      
      lon.diff <- data.frame(lon = m.lon, diff = NA, closest = NA)
      lon.diff$diff <- abs(lon.diff$lon - day.lon)
      closest.ref.lon.m2 <- match(x = min(lon.diff$diff, na.rm = T), table = lon.diff$diff)
      
      ###  now we extract all of the relevant model information 
      sal.dip.m1<- m.sal[closest.ref.lon.m1, closest.ref.lat.m1,,x]
      temp.dip.m1 <- m.temp[closest.ref.lon.m1, closest.ref.lat.m1,,x]
      bottom.dip.m1 <- m.btemp[closest.ref.lon.m1, closest.ref.lat.m1,x]
      
      sal.dip.m2<- m.sal[closest.ref.lon.m2, closest.ref.lat.m2,,x]
      temp.dip.m2 <- m.temp[closest.ref.lon.m2, closest.ref.lat.m2,,x]
      bottom.dip.m2 <- m.btemp[closest.ref.lon.m2, closest.ref.lat.m2,x]
      
      ## we save it down
      temp.model <- data.frame(SAL.m1 = sal.dip.m1,
                               SAL.m2 = sal.dip.m2,
                               TEMP.m1 = temp.dip.m1, 
                               TEMP.m2 = temp.dip.m2, 
                               BOTTOM.m1 = bottom.dip.m1, 
                               BOTTOM.m2 = bottom.dip.m2, 
                               modLAT.m1 = closest.lat$val,
                               modLAT.m2 = lat.diff$lat[closest.ref.lat.m2],
                               ctdLAT = day.lat,
                               modlon.m1 = closest.lon$val,
                               modlon.m2 = lon.diff$lon[closest.ref.lon.m2],
                                ctdLON = day.lon,
                               DATE = unique(ctd.from.day$DATE), 
                               modDEPTH = m.depth, 
                               UNIQ = paste(day.lat, day.lon, ctd.from.day$DATE[1], sep = ""))
      
      ### extract A - reinsert here
      
      one.month.data <- rbind(one.month.data, temp.model)
    #name.to.save.monthly <- paste("../Data/CMEMS_Monthly", year, month, ".Rdata", sep = "_")
    #save(one.month.data, file = name.to.save.monthly)
  }}

  
  ### now we can also try and create a master.df 
  
  all.model.data <- rbind(all.model.data, one.month.data)
  
  #name.to.save.all <- paste("../Data/CMEMS_All_File_", i, "_of", length(model.list), ".Rdata", sep = "")
  
  #save(all.model.data, file = name.to.save.all)
  nc_close(model)
  
}

#name.to.save.all <- paste("../Data/CMEMS_All4_File_", i, "_of", length(model.list), ".Rdata", sep = "")

#save(all.model.data, file = name.to.save.all)

print(name.to.save.all)



### INSERT A
### and now we are going to match it, becuase why the hell not

temp.model$ctdDEPTH <- NA
temp.model$ctdSAL <- NA
temp.model$ctdTEMP <- NA


### set up my data table for the matching process
# make separate depth variable as doesn't like it when reference a df
depths <- ctd.from.day$DEPTH
dt.ctdDepth <- data.table(depths, val = depths)
setattr(dt.ctdDepth, "sorted", "depths")
setkey(dt.ctdDepth, depths)

print("Now on to the matching stage")
#k <- 1
for (k in 1:length(temp.model$modDEPTH)) {
  # now we identify the closest CTD depth 
  
  mod.depth <- temp.model$modDEPTH[k]
  closest.depth <- dt.ctdDepth[J(mod.depth), roll = "nearest"] # closest lat is model lat 
  # we then find where the matching lat is (for indexing purposes)
  closest.ref.depth <- match(x = closest.depth$val, table = ctd.from.day$DEPTH)
  
  ## now we add in the comparison data
  
  temp.model$ctdDEPTH[k] <- closest.depth$val
  temp.model$ctdSAL[k] <- ctd.from.day$SAL_A[closest.ref.depth]
  temp.model$ctdTEMP[k] <- ctd.from.day$TEMP_P[closest.ref.depth]
  
  ## and then we store it 
  
  
}


## let's plot all model data to see if it looks weird? 
# use the CTD data from the first 20 models somehow and also plot that, to compare
# before and after the plotting episode 

### first step - isolates the dates we used (20 months of data)

extracted.dates <- unique(all.model.data$DATE)
ctd.extracted <- subset(master.df, master.df$DATE %in% extracted.dates)
ctd.extracted$latlon <- paste(ctd.extracted$LAT, ctd.extracted$LONG, sep = "")
ctd.extracted <- ctd.extracted[!duplicated(ctd.extracted$latlon),] # 384 locations

## method one lat-longs

latlon.m1 <- data.frame(lat = all.model.data$modLAT.m1, lon = all.model.data$modlon.m1, 
                        latlon = paste(all.model.data$modLAT.m1, all.model.data$modlon.m1, sep = ""))
latlon.m1 <- latlon.m1[!duplicated(latlon.m1$latlon),]

latlon.m2 <- data.frame(lat = all.model.data$modLAT.m2, lon = all.model.data$modlon.m2, 
                        latlon = paste(all.model.data$modLAT.m2, all.model.data$modlon.m2, sep = ""))
latlon.m2 <- latlon.m2[!duplicated(latlon.m2$latlon),]

ctd.matched <- data.frame(lat = all.model.data$ctdLAT, lon = all.model.data$ctdLON, 
                          latlon = paste(all.model.data$ctdLAT, all.model.data$ctdLON, sep = ""))
ctd.matched <- ctd.matched[!duplicated(ctd.matched$latlon),]


## right lets plot these and see how weird they look!

library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")
#install.packages("rgeos")
library(rgeos)
library(viridisLite) # this has colour palattes
library(RColorBrewer)
library(viridis)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

test <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = ctd.extracted, aes(x = LONG, y = LAT, col = "ctd.extracted"), size = 2, 
             shape = 18) +
  geom_point(data = latlon.m1, aes(x = lon, y = lat, colour = "model.m1"), size = 2, 
             shape = 20) +
  geom_point(data = latlon.m2, aes(x = lon, y = lat, colour = "model.m2"), size = 2, 
              shape = 18) +
  geom_point(data = ctd.matched, aes(x = lon, y = lat, col = "ctd.matched"), size = 2, 
             shape = 18) +
  scale_colour_gradientn(colours = turbo(n= 10)) +
  coord_sf(xlim = c(-20, -79), ylim = c(-45, -70), expand = FALSE)

test

### what if I didn't bother making them unique & just plotted them all? 


test2 <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = ctd.extracted, aes(x = LONG, y = LAT, col = "red"), size = 10, 
             shape = 18) +
  geom_point(data = all.model.data, aes(x = modlon.m1, y = modLAT.m1, colour = "blue"), size = 8, 
             shape = 20) +
  geom_point(data = all.model.data, aes(x = modlon.m2, y = modLAT.m2, colour = "purple"), size = 5, 
              shape = 18) +
  geom_point(data = all.model.data, aes(x = ctdLON, y = ctdLAT, col = "pink"), size = 2, 
             shape = 18) +
  #scale_colour_gradientn(colours = turbo(n= 10)) +
  coord_sf(xlim = c(-20, -79), ylim = c(-45, -70), expand = FALSE)

test2
