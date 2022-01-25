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


###### now, let's load in our two datasets - model data & ctd data

load("../Data/CTD_master_211108_v3_AllConverted.Rdata")

load("../Data/CMEMS_All4_File_92_of92.Rdata")

## let's plot the locations of the CTD data vs the locations of the model data

## first, create unique locations for CTDs otherwise it's quite a few points 

ctd.latlongs <- data.frame(lat = master.df$LAT, long = master.df$LONG, 
                           latlong = paste(master.df$LAT, master.df$LONG, sep = ""))

# remove duplicated rows
library(dplyr)
ctd.latlongs <- ctd.latlongs[!duplicated(ctd.latlongs),]

## and same with model

mod.latlongs <- data.frame(lat = all.model.data$modLAT, long = all.model.data$modLON, 
                           latlong = paste(all.model.data$modLAT, all.model.data$modLON, sep = ""))
  
mod.latlongs <- mod.latlongs[!duplicated(mod.latlongs),]


### now we do the same with the model ones - using a raw model file

load(file = "../Data/CMEMS_models_tocompare.Rdata")
library(ncdf4)

model <- nc_open(file = paste(to.models, model.list[1], sep = ""))
lats <- ncvar_get(model, "latitude")
longs <- ncvar_get(model, "longitude")

model.df <- data.frame(long = NA, lat = NA)

for (i in 1:length(lats)) {
  temp <- data.frame(long = longs, lat = lats[i])
  model.df <- rbind(model.df, temp)
}

### now we put it together - focus on a single degree square as whole thing is too dense


point.plot <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = ctd.latlongs, aes(x = long, y = lat, colour = "red"), size = 2, 
             shape = 18) +
  geom_point(data = mod.latlongs, aes(x = long, y = lat, colour = "blue"), size = 2, 
             shape = 20) +
 # geom_point(data = model.df, aes(x = long, y = lat, colour = "purple"), size = 2, 
#             shape = 18) +
  #scale_colour_gradientn(colours = turbo(n= 1)) +
  coord_sf(xlim = c(-20, -79), ylim = c(-45, -70), expand = FALSE)

point.plot

## so there is a very dense grid of model dots, and CTD dates (coming in blue), 
# and the 'model overlap' dots do not really overlap thhat much at all. 
# clearly something is going wrong with the matching process 
# so let's redo the matching process
# back to ExtractRelevantModelData_tweak.R

## actually, let's quickly plot something else@

mod.latlongs <- data.frame(lat = all.model.data$modLAT, long = all.model.data$modLON, 
                           latlong = paste(all.model.data$modLAT, all.model.data$modLON, sep = ""))

mod.latlongs <- mod.latlongs[!duplicated(mod.latlongs),]

mod.ctd.latlongs <- data.frame(lat = all.model.data$ctdLAT, long = all.model.data$ctdLON, 
                               latlong = paste(all.model.data$ctdLAT, all.model.data$ctdLON, sep = ""))

mod.ctd.latlongs <- mod.latlongs[!duplicated(mod.latlongs),]

###

matching.locations <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = mod.latlongs, aes(x = long, y = lat, colour = "red"), size = 2, 
             shape = 18) +
  geom_point(data = mod.ctd.latlongs, aes(x = long, y = lat, colour = "blue"), size = 2, 
             shape = 20) +
  # geom_point(data = model.df, aes(x = long, y = lat, colour = "purple"), size = 2, 
  #             shape = 18) +
  #scale_colour_gradientn(colours = turbo(n= 1)) +
  coord_sf(xlim = c(-20, -79), ylim = c(-45, -70), expand = FALSE)

matching.locations # ok so these do match up - that's a relief. 


###


## add the unque identifier to the master df

master.df$UNIQ <- paste(master.df$LAT, master.df$LONG, master.df$DATE, sep = "")

#### and now we match the CTD data to the model data and then we 'match' each CTD entry
# to the closest model depth


comparison <- data.frame(  SAL = NA, TEMP = NA, BOTTOM = NA, 
                           modLAT = NA , ctdLAT = NA,
                           modLONG = NA, ctdLON = NA,
                           DATE = NA, 
                           modDEPTH = NA, 
                           UNIQ = NA, 
                           ctdDEPTH = NA,
                           ctdSAL = NA, 
                           ctdTEMP = NA)


# LETS GO THROUGH IT A SINGLE DAY AT A TIME

mod.uniq <-  unique(all.model.data$UNIQ)
ctd.uniq <- unique((master.df$UNIQ))
uniqs <- ctd.uniq[ctd.uniq %in% mod.uniq]
#uniqs2 <- mod.uniq[mod.uniq %in% ctd.uniq]

for (i in 1:length(uniqs)) {
  
  print(paste("UNIQ ID ", i, " of ", length(uniqs)))

  temp.model <- subset(all.model.data, all.model.data$UNIQ == uniqs[i])
  temp.ctd <- subset(master.df, master.df$UNIQ == uniqs[i])
  
  temp.model$ctdDEPTH <- NA
  temp.model$ctdSAL <- NA
  temp.model$ctdTEMP <- NA
  
  ### set up my data table for the matching process
  # make separate depth variable as doesn't like it when reference a df
  depths <- temp.ctd$DEPTH
  dt <- data.table(depths, val = depths)
  setattr(dt, "sorted", "depths")
  setkey(dt, depths)
  
  ## going to keep the model data and then find closest ctd data to verify it
  
  for (j in 1:length(temp.model$modDEPTH)) {
    # now we identify the closest CTD depth 
    
    mod.depth <- temp.model$modDEPTH[j]
    closest.depth <- dt[J(mod.depth), roll = "nearest"] # closest lat is model lat 
    # we then find where the matching lat is (for indexing purposes)
    closest.ref.depth <- match(x = closest.depth$val, table = temp.ctd$DEPTH)
    
    ## now we add in the comparison data
    
    temp.model$ctdDEPTH <- closest.depth$val
    temp.model$ctdSAL <- temp.ctd$SAL_A[closest.ref.depth]
    temp.model$ctdTEMP <- temp.ctd$TEMP_P[closest.ref.depth]
    
  }
  
  #now we save it down!
  
  comparison <- rbind(comparison, temp.model)
  
}

save(comparison, file = "../Data/CMEMS_modCTD_Comparison.Rdata")


# do I compare the 

### NEXT STEPS: why did we lose a bunch of model data?
## CLEANING: some matches may be way off (especially the deepest ones) - come up with 
# something clever to detect and remove these 'matches'
# do some R squared stuff to compare accross: time, depths & whole data frame

unmatched <- mod.uniq[!(mod.uniq %in% ctd.uniq)]

unm <- subset(all.model.data, all.model.data$UNIQ %in% unmatched)

length(uniqs) * 50 + 1
