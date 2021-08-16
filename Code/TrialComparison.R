#/usr/bin/Rscript --vanilla

## In this script I am opening a single CTD data file, getting the temperature, salinity, depth, location & time
# and then I will open up the COPERNICUS model, find the grid cell that corresponds to the location of the CTD at the time of the CTD
## and compare the model's data with what the CTD has recorded

## Step one: extracting the useful information from a CTD file


#####
#### SET UP WORKING ENVIRONMENT

graphics.off()
rm(list = ls())

#getwd()
# standard pathway 
#pathway.to.linux <- "../../../oenon/AppData/Local/Packages/CanonicalGroupLimited.UbuntuonWindows_79rhkp1fndgsc/LocalState/rootfs/home/oslin/"

#save(pathway.to.linux, file = "pathway.Rdata")
load("pathway.Rdata")

setwd(pathway.to.linux)

# now set actually locally
setwd("Chapter1/Code/")


library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
#install.packages("chron")
library(chron)


######
##### LOAD IN MY LIST OF CTD FILES & SELECT ONE 


## first we load in a ctd data
# load up the df of relevant files

load("../Data/BODC/RelevantFiles.Rdata")

# and restrict to nc files

all.df <- all.df[all.df$relevant == T,]


# pick a random file to open

#rf <- as.numeric(sample(1:length(all.df$file), 1, replace = T))
rf <- as.numeric(1291)

path.to.data <- "../Data/BODC/AllCDTData/"

data <- nc_open(file = paste(path.to.data, all.df$file[rf], sep = ""))

#
print(data)

########
### USING THE variable-get function, let's have a look at the variables we want

ctd.date <- ncvar_get(data, "TIME")
data$var$TIME$units # get the origin date
ctd.date.normal <- month.day.year(jul = ctd.date, origin = c(month = 1, day = 1, year = -4713)) # convert to something useful
ctd.date.vector <- paste(ctd.date.normal$year, ctd.date.normal$month, ctd.date.normal$day, sep = "-")

depth <- ncvar_get(data, "SDN_BOT_DEPTH")
location.lat <- ncvar_get(data, "LATITUDE")
location.lon <- ncvar_get(data, "LONGITUDE")
salinity <- ncvar_get(data, "PSALPR01")
temp <- ncvar_get(data, "TEMPST01")

# so I don't think this works - I have 198 temperature values, and a lat & a long, 
# but I don't have any depth data or salinity data 

temp
print(data)

# rf - 1291 for the below

#EUREKA
# SDN_BOT_DEPTH[INSTANCE]  is the bathymetric depth at the site (in m), NOT the depth per measurements
# which is why there is only one data point

# PRES (long_name: Pres_Z) is the sea water pressure due to sea water - which should be the depth
# measured in decibars

# AHSFZZ01 (Height_aboveBed) is the height relative to the bed surface in the water body

# CNDCST01 (CTDCond) is the conductivity, siemens per metre

# POTMCV01 (long_name: WC_Potemp) potential temp of water in deg C

#PSALST01 salinity of water, uses the time, pressure lat and long


press <- ncvar_get(data, "PRES")

# 1 decibar = 1.01974m water

conversion <- 1.01974

depth.m <- press*conversion

hab <- ncvar_get(data, "AHSFZZ01")
# CONVERT PRESSURE TO DEPTH

pot.temp <- ncvar_get(data, "POTMCV01")

conduc  <- ncvar_get(data, "CNDCST01")

sal <- ncvar_get(data, "PSALST01")

# 

print(data)

print(press)

ncvar_get(data, "SDN_STATION")

######
## STORE ALL TOGETHER

ctd.relvant <- data.frame(DEPTH = depth.m, 
                          TEMP = pot.temp, Conduc = conduc, SAL = sal)

##############
###########
#### OK NOW WE HAVE THE RELEVANT DATA! Now we load in the model
# try and find the same location & data and we COMPARE!



########

# so I am going to download a 'trial' section of copernicus just with three days
# and a narrow margin, just to make my life easier

# downloaded just one degree squared (-55 to -56, -41 to -42)
# and one month (February 2016), but all depths
# captured salinity, temperature 


model <- nc_open(file = "../Data/CMEMS/OneDegreeOneMonthSnippet.nc.nc")

print(model) # lets get that metadata

# let's take a look at the dimensions & time of our model

m.lon <- ncvar_get(model, "longitude") #dim = 13 
m.lat <- ncvar_get(model, "latitude") # dim = 13
m.depth <- ncvar_get(model, "depth") # dim = 50
m.time <- ncvar_get(model, "time") # this is in Julian days
m.t.units <- ncatt_get(model, "time", "units")



dim(m.depth) # check dimensions of all (answers above)

# look at the depth - we only have CTD data for the first 101m depth. 
# so, we only need depth for the first


m.depth.101 <- subset(m.depth, m.depth <110)
# went to 110 so it captures the boundary of the layer including the 
# final data (I think)

model$var

# lets look at salinity, here called "so"
m.salinity <- ncvar_get(model, "so")
dim(m.salinity) # 4 dimensional model - 13 lat * 13 lon * 50 depth * 30 days

# now lets whittle this down
# target long & lat = location.lat & location.lon

# need to convert the 4th dimension to normal dates
# need to divide by 24 as it's in hours, and then do the julian conversion
m.time.convert.j <- m.time/24
m.time.convert <- month.day.year(jul = m.time.convert.j, origin = c(month = 1, day = 1, year = 1950)) 
m.time.vector <- paste(m.time.convert$year, m.time.convert$month, m.time.convert$day, sep = "-")


### so - get the index for the correct date

m.time.vector[1]

for (i in 1:length(m.time.vector)) {
  if (m.time.vector[i] == ctd.date.vector) {
    vec <- i 
    print(paste("Found it! It's: ", i, sep = " "))
  }
}


## so - the right day is slice EIGHT of our model!

m.salinity.day <-m.salinity[,,,8]

# we also know the depth's we're looking for, so let's slice for that too

m.s.day.depth <- m.salinity.day[,,1:length(m.depth.101)]

dim(m.s.day.depth) # now we have one day, 13 depths, but still 13 lat & longs

m.lat

m.s.day.depth[1,,]

for (i in 1:length(m.lat)) {
  if (m.lat[i] < location.lat && m.lat[i + 1] > location.lat) {
    lat.slice <- i 
    print(paste("Found it! It's: ", i, sep = " "))
  }
}

# lat slice = 9

for (i in 1:length(m.lon)) {
  if (m.lon[i] < location.lon && m.lon[i + 1] > location.lon) {
    lon.slice <- i 
    print(paste("Found it! It's: ", i, sep = " "))
  }
}

### so the data I'm comparing are:

sal.slice <- m.s.day.depth[lat.slice, lon.slice,]

plot(sal.slice)

### OK NOW WE NEED TO MATCH THE SLICES TO THE ACTUAL DATA

## PLOTTING THE DEPTH CATEGORIES

plot(ctd.relvant$DEPTH)

points(m.depth.101)


# let's pair depth & salinity for both of them 

graphics.off()

plot(y = sal.slice, x = m.depth.101, 
     pch = 19, col = "blue", 
     ylim = c(33.6,34), 
     ylab = "Salinity (ppm)",
     xlab = "depth (m)")

points(x = ctd.relvant$DEPTH, y = ctd.relvant$SAL, col = "red")

legend(legend =c("CMEMS output", "CTD data"), x = 80, y = 33.7, 
       col = c("blue", "red"), pch = c(19,1))


# so, we have 50 data points, but only 23 depth slices
# so, let's 'match' them 

comparison.df <- data.frame(model.depth = m.depth.101, model.salinity = sal.slice, 
                            low.ctd.depth = NA, higher.ctd.depth = NA,
                              low.ctd.sal= NA, higher.ctd.sal = NA, mean.ctd.sal = NA)


# going to try two approaches - 
# 1) using the CLOSEST model data to check the CTD data
# 2) using a mean of the CTD data between model buckets !?

# only using SOME of the CTD data 
  
for (i in 1:length(m.depth.101)) {
  print(paste("Matching depth", i, "of", length(m.depth.101), sep = " "))
  for (j in 1:length(ctd.relvant$DEPTH)) {
    if (m.depth.101[i] > ctd.relvant$DEPTH[j]) {
      comparison.df$low.ctd.depth[i] <- ctd.relvant$DEPTH[j]
      comparison.df$higher.ctd.depth[i] <- ctd.relvant$DEPTH[j+1]
    }
  }}

comparison.df$higher.ctd.depth[23] <- comparison.df$low.ctd.depth[23]
# now work out which one is closer

comparison.df$ctd.closest <- NA
comparison.df$HiLo <- NA
comparison.df$SalofClosestDepth <- NA

# starting from 2 as the first one is too shallow and pretty close 
for (i in 2:length(comparison.df$model.depth)) {
  if ((sqrt((comparison.df$model.depth[i] - comparison.df$low.ctd.depth[i])**2) < 
           sqrt((comparison.df$model.depth[i] - comparison.df$higher.ctd.depth[i])**2)) == T)  {
      
    comparison.df$HiLo[i] <- "Lo" 
    sal.ref <- as.numeric(match(comparison.df$low.ctd.depth[i], ctd.relvant$DEPTH))
    comparison.df$ClosestSalinity[i] <- ctd.relvant$SAL[sal.ref]
    print("Lo")
    }
  else {
    comparison.df$HiLo[i] <- "Hi"
    sal.ref <- as.numeric(match(comparison.df$low.ctd.depth[i], ctd.relvant$DEPTH))
    comparison.df$ClosestSalinity[i] <- ctd.relvant$SAL[sal.ref + 1] 
    print("Hi")}
}

### so - we don't have salinity data for the first two dips, which is why the top
# 4 values are 'NA'

# manually adding in matches for top & bottom (fix later)
ctd.relvant[52,] <- ctd.relvant[51,]
comparison.df$low.ctd.depth[1] <- comparison.df$low.ctd.depth[2]
comparison.df$low.ctd.depth[23] <- comparison.df$low.ctd.depth[2]

comparison.df$HiLo[1] <- "Hi"
comparison.df$HiLo[23] <- "Lo"

## so now we're going to pull the salinity associated with the CTD depth
# we're then going to work out the absolute difference, percentage difference 
# and whether it's 'higher' or 'lower'

comparison.df$PercentDiff <- NA
comparison.df$AbsoluteDiff <- NA

# ABSOLUTE DIFF = HOW MUCH DOES THE MODEL DIFFER FROM THE REAL DATA
# PERCENT = BY HOW MUCH OF THE REAL VALUE DOES THE MODEL DIFFER 
# USE THE REAL DATA AS THE BENCHMARK AS THIS WILL BE THE CONSISTANT ONE
# ACROSS THE DIFFERNET COMPARIOSNS.
comparison.df$AbsoluteDiff <- comparison.df$ClosestSalinity - comparison.df$model.salinity
# make positive & check percentage difference

comparison.df$PercentDiff <- sqrt(comparison.df$AbsoluteDiff **2)/comparison.df$ClosestSalinity*100

# the different isn't that big

# CALCAULTE ROOT MEAN SQUARE ERROR FOR THESE VALUES
#sqrt(mean((data$actual - data$predicted)^2))


RMS <- sqrt(mean((comparison.df$ClosestSalinity[5:23] - 
                   comparison.df$model.salinity[5:23])**2))

# can also do this using an R package!

#install.packages("Metrics")
library("Metrics")
#rms.package <- rmse(data$actual, data$predicted)
rms.package <- rmse(comparison.df$ClosestSalinity[5:23],
                      comparison.df$model.salinity[5:23])

# they are the same! F A B. 

######################################### NOW REPEAT FOR TEMPERATURE??



# as the first four depths weren't matched/ didn't have CTD salinity measurements
# just compared the locations where dip data was matched with model data 


## QUESTION FOR EMMA- do I maximise the use of the CTD data by 
# using that as the framework (all 50 dips) and then matching the closest
# depth of the model data to that? 


### NEXT STEPs:  I could also just try plotting the model data to 
# work out which way round it wraps



