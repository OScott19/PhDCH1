### this is simply a tidied version of the CTD_masterdf_exploratory.R file
# this file calls on other data files that haave been generated by previous scripts
# where this has happened, the name of the script is noted above


print("Setting up environment")
######################
### set the environment


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

counter <- 0
  
  
master.df <- data.frame(I = NA, DATE = NA, JULIANINFO = NA, 
                        PRESS = NA, DEPTH = NA, PRESSTXT = NA, 
                        TEMP_P = NA, TEMP_A = NA, TEMP_UNIT = NA, 
                        SAL_P = NA, SAL_A = NA, SAL_UNIT = NA, 
                        LAT = NA, LONG = NA, CRUISEID = NA, MAXDEPTH = NA, 
                        FILE = NA)

holding.df <-  master.df

######################
################ LOOP
######################

print("Starting the loop")

for (i in 1:length(r.df$file)) {
  # to ensure we're not carrying over data, we resset the holding data frame to be empty each time
  holding.df <- data.frame(I = NA, DATE = NA, JULIANINFO = NA, 
                           PRESS = NA, DEPTH = NA, PRESSTXT = NA, 
                           TEMP_P = NA, TEMP_A = NA, TEMP_UNIT = NA, 
                           SAL_P = NA, SAL_A = NA, SAL_UNIT = NA, 
                           LAT = NA, LONG = NA, CRUISEID = NA, MAXDEPTH = NA, 
                           FILE = NA)
  
  # we then open the .nc file (one at a time)
  # w.data the "w." prefix here is "working" - ie will change every time
  w.data <- nc_open(file = paste(path.to.data, r.df$file[i], sep = ""))
  # store variables
  
  # WORK OUT HOW THE DEPTH HAS BEEN STORED - AS PRESSURE OR AS DEPTH? ?
  # if they have both - then use depth (more accurate?) but also record pressure
  
  
  if(length(w.data$var$PRES) > 0) {
    w.pressure <- ncvar_get(w.data, "PRES")
    # we now re-initialise the dataframe, this time using the single re-extracted variable
    #  this makes the length of the df correct!
    holding.df <- data.frame(I = NA, DATE = NA, JULIANINFO = NA, 
                             PRESS = w.pressure, DEPTH = NA, PRESSTXT = NA, 
                             TEMP_P = NA, TEMP_A = NA, TEMP_UNIT = NA, 
                             SAL_P = NA, SAL_A = NA, SAL_UNIT = NA, 
                             LAT = NA, LONG = NA, CRUISEID = NA, MAXDEPTH = NA, 
                             FILE = NA)
    holding.df$PRESSTXT <- w.data$var$PRES$units
  }
  
  # however, it might be that it's stored directly as 'depth' (which is GREAT)
  
  if (length(w.data$var$DEPTH) > 0) {
    w.depth <- ncvar_get(w.data, "DEPTH")
    holding.df <- data.frame(I = NA, DATE = NA, JULIANINFO = NA, 
                              PRESS = NA, DEPTH = w.depth, PRESSTXT = NA, 
                              TEMP_P = NA, TEMP_A = NA, TEMP_UNIT = NA, 
                              SAL_P = NA, SAL_A = NA, SAL_UNIT = NA, 
                              LAT = NA, LONG = NA, CRUISEID = NA, MAXDEPTH = NA, 
                              FILE = NA)
    
  }
  
  if ((length(w.data$var$PRES) > 0) & (length(w.data$var$DEPTH) > 0)) {
    holding.df$PRESS <- w.pressure
    holding.df$PRESSTXT <- w.data$var$PRES$units
  } 
  
  # and now we insert the rest of the data
  holding.df$I <- i
  holding.df$FILE <- r.df$file[i]
  try(holding.df$DATE <- ncvar_get(w.data, "TIME"), silent = F, outFile = "Error - date")
  # now put in the data that we will use to convert the Julian date
  try(holding.df$JULIANINFO <- w.data$var$TIME$units)
  #holding.df$CONDUCTIVITY <- ncvar_get(w.data
  try(holding.df$LAT <- ncvar_get(w.data, "LATITUDE"))
  try(holding.df$LONG <- ncvar_get(w.data, "LONGITUDE"))
  try(holding.df$MAXDEPTH <- ncvar_get(w.data, "SDN_BOT_DEPTH"), silent = F, outFile = "Error - max depth")
  
  ## different types of salinity need to be captured 
  
  if (length(w.data$var$PSALPR01) > 0) {
    holding.df$SAL_P <- ncvar_get(w.data, "PSALPR01")
    holding.df$SAL_UNIT <- w.data$var$PSALPR01$units }
  
  if (length(w.data$var$SSALPR01) > 0) {
    
    holding.df$SAL_A <- ncvar_get(w.data, "SSALPR01")
    holding.df$SAL_UNIT <- w.data$var$SSALPR01$units }
  
  
  ## and different types of temperature
  
  # ACTUAL TEMPERATURES
  
  if (length(w.data$var$TEMPST01) > 0) {
    holding.df$TEMP_A <- ncvar_get(w.data, "TEMPST01")
    holding.df$TEMP_UNIT <- w.data$var$TEMPST01$units }
  
  
  if (length(w.data$var$TEMPCC01) > 0) {
    holding.df$TEMP_A <- ncvar_get(w.data, "TEMPCC01")
    holding.df$TEMP_UNIT <- w.data$var$TEMPCC01$units }
  
  if (length(w.data$var$TEMPCU01) > 0) {
    holding.df$TEMP_A <- ncvar_get(w.data, "TEMPCU01")
    holding.df$TEMP_UNIT <- w.data$var$TEMPCU01$units }
  
  
  ##POTENTIAL TEMPERATURES
  
  if (length(w.data$var$TEMPPR01) > 0) {
    
    holding.df$TEMP_P <- ncvar_get(w.data, "TEMPPR01")
    holding.df$TEMP_UNIT <- w.data$var$TEMPPR01$units }
  
  if (length(w.data$var$POTMCV01) > 0) {
    
    holding.df$TEMP_P <- ncvar_get(w.data, "POTMCV01")
    holding.df$TEMP_UNIT <- w.data$var$POTMCV01$units }
  
  if (length(w.data$var$POTMCV02) > 0) {
    
    holding.df$TEMP_P <- ncvar_get(w.data, "POTMCV02")
    holding.df$TEMP_UNIT <- w.data$var$POTMCV02$units }
  
  
  try(holding.df$CRUISEID <- ncvar_get(w.data,"SDN_CRUISE"))
  
  ### once all data gathered, then we concatenate to the master df 
  
  
  master.df <- rbind(master.df, holding.df)
  
  nc_close(w.data)
  
  
  if (i %% 200 == 0) {
    print(i)  
    
  }
  
  
}

####### THIS SHOULD RESULT IN: SINGLE Rdata files that contain only the information that we are interestd in 

print("Converting data to a sensible form")

### We now want to put this data into a more useful format
# firstly, we Identify if any rows are FULLY NAs, and we will remove them

# check how many rows are all NAs
# get rid of rows that are ALL nas

row.rm <- 1

#for (j in 1:length(master.df$I)) {
#  row <- master.df[j,]
#  if (length(row[is.na(row)]) == 17) {
#    row.rm <- c(row.rm, j) }
#}

master.df <- master.df[-row.rm,]


############################
# now, let's convert the data that we have into a more useful format


## starting with the dates - transform from a Julian date to a normal one

# first of all, check they all have the same Julian conversion

length(unique(master.df$JULIANINFO)) # this should be 1. If >1, then 
# will need to do the following in two steps

unique(master.df$JULIANINFO) #find out what the Julian origin date is

library(chron)
# let's convert all of the dates then
ctd.date.normal <- month.day.year(jul = master.df$DATE, origin = c(month = 1, day = 1, year = -4713)) # convert to something useful
ctd.date.vector <- paste(ctd.date.normal$year, ctd.date.normal$month, ctd.date.normal$day, sep = "-")
# and add this into the master

master.df$JDATE <- master.df$DATE
master.df$DATE <- ctd.date.vector




### converting pressure to depth (as required)

library(oce)
save.depths <- master.df$DEPTH
master.df$DEPTH[is.na(master.df$PRESS) == F ] <- oce::swDepth(pressure = master.df$PRESS[is.na(master.df$PRESS) == F ], 
                                                              latitude = master.df$LAT[is.na(master.df$PRESS) == F ], 
                                                              eos = "unesco")

checker <- data.frame(ORIG = save.depths, NEW = master.df$DEPTH)


##### ALWAYS SAVE!

# will then need to save the final version of the master df

save(master.df, file = "../Data/CTD_master_211023_everydamnTemp.Rdata")
# this is now the wrong file format - make it yy-mm--dd next time


print("Data saved, script finished")
