# in this script we are going to: 
# open each CTD data file one by one
# extract only the useful information: 
# date, text to transform date, pressure (& description), conductivity
# temperature, lat, long, cruise ID (to check for duplicates)

# this is going to be stored in a master DF
# we can then 


# extract the pertinant information from each 'read' and then
# concatenate to the bottom of the master DF
# we can then do some fun-kay analyses on 
#what we even have!



#####
#### SET UP WORKING ENVIRONMENT

graphics.off()
rm(list = ls())

#getwd()
# standard pathway 
#pathway.to.linux <- "../../../oenon/AppData/Local/Packages/CanonicalGroupLimited.UbuntuonWindows_79rhkp1fndgsc/LocalState/rootfs/home/oslin/"
pathway.to.linux <- "../../../oenon/AppData/Local/Packages/CanonicalGroupLimited.Ubuntu20.04onWindows_79rhkp1fndgsc/LocalState/rootfs/home/oslin/"


#save(pathway.to.linux, file = "pathway.Rdata")
load("pathway.Rdata")

setwd(pathway.to.linux)

# now set actually locally
setwd("PhDCH1/Code/")


# and load in my DF of all the relevant files

all <-  read.csv("../Data/BODC/AllCDTData/largerlist.txt")

all.df <- data.frame(file = all$X1009513.html, end = NA, relevant = NA)

end <- strsplit(all.df$file, split = ".", fixed = T)

end[[1]][2] # end is now the second part of a list

for (i in 1:length(end)) {
  all.df$end[i] <- end[[i]][2]
} 


all.df$relevant <- F
all.df$relevant[all.df$end == "nc"] <- T

r.df <- all.df[all.df$relevant == T,]

save(all.df, r.df, file = "../Data/BODC/MoreRelevantFiles.Rdata") # saved so I don't need to do this again
load("../Data/BODC/MoreRelevantFiles.Rdata")


#### 


# what data am I saving:
# date, text to transform date, pressure (& description), conductivity
# temperature, lat, long, cruise ID (to check for duplicates)

master.df <- data.frame(I = NA, DATE = NA, JULIANINFO = NA, PRESS = NA, 
                       PRESSTXT = NA, CONDUCTIVITY = NA, 
                       TEMPERATURE = NA, SALINITY = NA, LAT = NA, 
                       LONG = NA, CRUISEID = NA, MAXDEPTH = NA, 
                       DEPTH = NA, FILE = NA)

holding.df <-  master.df




# RIGHT LETS LOAD STUFF UP 

## PACKAGES FIRST 
install.packages(c("ncdf4", "raster", "rgdal", "ggplot2", "chron"))
install.packages(c("rio", "readr", "data.table", "feather"))

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
#install.packages("chron")
library(chron)

path.to.data <- "../Data/BODC/AllCDTData/"

counter <- 4

for (i in 2001:length(r.df$file)) {
  # to ensure we're not carrying over data, we resset the holding data frame to be empty each time
  holding.df <- data.frame(I = NA, DATE = NA, JULIANINFO = NA, PRESS = NA, 
                           PRESSTXT = NA, CONDUCTIVITY = NA, 
                           TEMPERATURE = NA, SALINITY = NA, LAT = NA, 
                           LONG = NA, CRUISEID = NA, MAXDEPTH = NA, 
                           DEPTH = NA, FILE = NA)
  
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
    holding.df <- data.frame(I = NA, DATE = NA, JULIANINFO = NA, PRESS = w.pressure, 
                              PRESSTXT = NA, CONDUCTIVITY = NA, 
                              TEMPERATURE = NA, SALINITY = NA, LAT = NA, 
                              LONG = NA, CRUISEID = NA, MAXDEPTH = NA, 
                              DEPTH = NA, FILE = NA)
    holding.df$PRESSTXT <- w.data$var$PRES$units
  }
  
  # however, it might be that it's stored directly as 'depth' (which is GREAT)
   
  if (length(w.data$var$DEPTH) > 0) {
    w.depth <- ncvar_get(w.data, "DEPTH")
    holding.df <- data.frame(I = NA, DATE = NA, JULIANINFO = NA, PRESS = NA, 
                             PRESSTXT = NA, CONDUCTIVITY = NA, 
                             TEMPERATURE = NA, SALINITY = NA, LAT = NA, 
                             LONG = NA, CRUISEID = NA, MAXDEPTH = NA, 
                             DEPTH = w.depth, FILE = NA)
    
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
  try(holding.df$TEMPERATURE <- ncvar_get(w.data, "TEMPST01"))
  try(holding.df$SALINITY <- ncvar_get(w.data, "PSALPR01"), silent = T)
  try(holding.df$CRUISEID <- w.data$id)
  
  ### once all data gathered, then we concatenate to the master df 
  
  
  master.df <- rbind(master.df, holding.df)
  
  
  if (i %% 100 == 0) {
    print(i)  
    
  
  
  if (object.size(master.df) >50000000 ) {
    counter <- counter + 1
    nametosave <- paste("../Data/LongMasterDF_", counter, (".Rdata"), sep = "")
    save(master.df, file = nametosave)
    
    # then reset the master.df
    
    master.df <- data.frame(I = NA, DATE = NA, JULIANINFO = NA, PRESS = NA, 
                            PRESSTXT = NA, CONDUCTIVITY = NA, 
                            TEMPERATURE = NA, SALINITY = NA, LAT = NA, 
                            LONG = NA, CRUISEID = NA, MAXDEPTH = NA, 
                            DEPTH = NA, FILE = NA)
    
    }    
    }
  
}

rm(holding.df, r.df, w.data)

test <- NA

saveRDS(master.firsthalf, file = "C:/Users/oenon/CTD_master_larger.Rdata")
save(master.df, file = "../Data/masterDF_larger")

rm(master.df)
master.firsthalf <- master.df[1:(length(master.df$I)/2),]
master.secondhalf <- master.df[(length(master.df$I)/2):length(master.df$I),]

object.size(master.secondhalf)

length(master.df$DEPTH[master.df$DEPTH > 1000])
max(master.df$DEPTH, na.rm = T)

#load("../Data/CTD_masterDF.Rdata")

length(unique(master.df$PRESS))
length(unique(master.df$I)) # now up to 2046 files!! increase of c.1500, but 900 to go
length(unique(r.df$file)) # 2982 - great 
length(unique(master.df$FILE)) # also only 422

# next step - find out why we can't load in those other 2500 files?

missing <- r.df$file[r.df$file %in% master.df$FILE == F] #937 unentered files

length(missing) + length(unique(master.df$FILE)) # yes we have the correct length


# so, let's try and load in one of these missing files

# stepping through the code in the loop
i <- 423
#this loads correctly: 
w.data <- nc_open(file = paste(path.to.data, r.df$file[i], sep = ""))
# however this doesn't: 
w.pressure <- ncvar_get(w.data, "PRES") 
# so the pressure isn't recorded as "PRES"
print(w.data) #lets take a  look at the variables in the file

w.data$var$DEPTH
# so, the depth here is stored as Depth rather than pressure
# so, we need to add an if statement to our loop

length(w.data$var$DEPTH) > 0
length(w.data$var$PRES) > 0



