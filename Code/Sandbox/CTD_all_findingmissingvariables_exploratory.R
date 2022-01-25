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


#################

# SEE LINE 179 FIRST

######################
# prime the counter and the dataframes
path.to.data <- "../Data/BODC/AllCDTData/"






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

# so it appears someetimes we just don't have bathymetric data. 
# check other entries 
# not going to check one with a known mishap though
length(master.df$I[is.na(master.df$I)]) # None!
length(master.df$DATE[is.na(master.df$DATE)]) # also none
length(master.df$DATE[is.na(master.df$DEPTH)]) # quite a few 
length(master.df$JULIANINFO[is.na(master.df$JULIANINFO)])
length(master.df$TEMPERATURE[is.na(master.df$TEMP_A)]) # quite a few - 1087995
length(master.df$TEMPERATURE[is.na(master.df$TEMP_P)])
length(master.df$TEMPERATURE_UNIT[is.na(master.df$TEMPERATURE_UNIT)]) # more than before - weird! - 1413952
length(master.df$LAT[is.na(master.df$LAT)]) # back to None! 
length(master.df$LONG[is.na(master.df$LONG)]) # back to None phew 
length(master.df$CRUISEID[is.na(master.df$CRUISEID)]) # back to None phew 
length(master.df$FILE[is.na(master.df$FILE)]) # back to None phew 
length(master.df$SALINITY[is.na(master.df$SALINITY)]) # 2709321 
length(master.df$SALINITY_UNIT[is.na(master.df$SALINITY_UNIT)]) # 2708242
### so it turns out that we have quite a lot more missing temperature data than I thought
# and we also have quite a lot of missing salinity data. not good!


#### let's have a look at some of the files with missing temperature data, and see what's going on

nc_close(w.data)

no.temp <- master.df$FILE[is.na(master.df$TEMPERATURE) == T] # matches number above

no.temp <- unique(no.temp) # 859 files - ok 

w.data <- nc_open(file = paste("../Data/BODC/AllCDTData/", no.temp[1], sep = ""))
# so we have data, under the TEMPST01 

data <- ncvar_get(w.data, "TEMPST01") # ok it has values here

data.inmaster <- master.df$TEMPERATURE[master.df$FILE == no.temp[1]] # the same??
min(data.inmaster)
min(data)
# however it *contains* an NA somewhere
temp.table <- as.data.frame(table(data))
sum(temp.table$Freq) # contains a single NA somewhere
data.wholeclip <- master.df[master.df$FILE == no.temp[1],] 
# and it appears this NA is from meter 92, which inexplicably has no data in it. 


#### TO CONTINUE 
### WORK OUT WHY I HAVE SO MANY MISSING TEMPERATURES
# REPLACE NA WITH SOMETHING AND THEN LOOP THROUGH EACH FILE
# AND COUNT THE NUMBER OF INSTANCES OF NA
# AND THEN LOOK INTO THOSE FILES

# THEN REPEAT THIS WITH SALINITY INFORMATION
# DECIDE IF ITS WORTH REMOVING ENTRIES WITHOUT TEMP & SALINITY DATA

####################################
#####################################

# ok need to convert from practical to in-situ salinity before I can
# convert the pressure (as per the equation requirement below)


# let's only use the 
practical.sals <- subset(master.df$SAL_P, is.na(master.df$SAL_P) == F)
actual.sals <- subset(master.df$SAL_A, is.na(master.df$SAL_A) == F)

master.df$LONG360 <- master.df$LONG + 360 # needed for marelac to run! 

p2aS <-  marelac::convert_PStoAS(S = master.df$TEMP_P[subset(master.df$SAL_P, is.na(master.df$SAL_P) == F)], 
                                 #P = master.df$PRESS[master.df$SALINITY_UNIT == "Dmnless"], 
                                 lat = master.df$LAT[subset(master.df$SAL_P, is.na(master.df$SAL_P) == F)], 
                                 lon = master.df$LONG360[subset(master.df$SAL_P, is.na(master.df$SAL_P) == F)],
                                 Ocean = "Southern")

## this is takinga bloody long time to run
## if works - only convert one way 




lats <- as.vector(master.df$LAT[master.df$SALINITY_UNIT == "Dmnless"])
lats <- lats[is.na(lats) == F]

sal.measurements <- as.data.frame(table(master.df$SALINITY_UNIT))

min(lats)
max(lats)

length(lats[is.na(lats)])

max(master.df$LAT[master.df$SALINITY_UNIT == "Dmnless"])
min(master.df$LAT[master.df$SALINITY_UNIT == "Dmnless"])

store <- master.df$LAT[master.df$SALINITY_UNIT == "Dmnless"]

store.sorted <- sort(store, decreasing = T)






#####################################
##################################
#####################################


# HOWEVER - only <900 files are flagged as containing an NA, and I have nearly a million entries
# so there must be a few with a hell of a lot


print(w.data)




## Ok - on to standardisation
length(unique(master.df$TEMPERATURE_UNIT)) # two different ways of measuring temp - degC and NA







# lets save down our new file

save(master.df, file = "../Data/CTD_master_normaldatedepth.Rdata")

# ok so now we have  a data.frame with dates corrected, depths corrected, singular temp

# last one: salinity

length(unique(master.df$SALINITY_UNIT)) #dmless (dimensionless), NA and ppt

# want to convert between salinities so lets check the DMless vs the ppt metadata

library(ncdf4)

sal.files <- unique(subset(master.df$FILE, master.df$SALINITY_UNIT == "ppt"))
funky.files <- unique(subset(master.df$FILE, master.df$SALINITY_UNIT == "Dmnless"))

w.data <- nc_open(file = paste("../Data/BODC/AllCDTData/", sal.files[1], sep = ""))

# ok so this has ppt
print(w.data)
# ppt = Salinity of the water body by conductivity cell
# also called UspSal

## right so what about the other one
nc_close(w.data)

w.data <- nc_open(file = paste("../Data/BODC/AllCDTData/", funky.files[1], sep = ""))
print(w.data)
# Practical salinity of the water body by conductivity cell and computation using UNESCO 1983 algorithm
# so - what is difference between salinity & practical salinity? 

# it would appear "for all practical purposes" that salinity in PSU (dimensionless)
# has the same numerical value as saility in ppt

# however we can use the merlac package here!

library(marelac)



#### converting in-situ temperature to potential temperature

install.packages("gsw")

## try this one

gsw_pt_from_t



##################################
####################################
##################################
# SUCCESSFUL CODE ZONE!!
##################################
####################################
##################################

# EXPLORATORY CODE THAT WORKED- BEEN TIDIED AND MOVED INTO THE 'TIDY' SCRIPT



# check how many rows are all NAs
# get rid of rows that are ALL nas

row.rm <- c()

for (j in i:length(master.df$I)) {
  row <- master.df[j,]
  if (length(row[is.na(row)]) == 17) {
    row.rm <- c(row.rm, j) }
}



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

pressures <- data.frame(PRES = master.df$PRESSS, UNIT = master.df$, SAL = master.df$SALINITY, 
                        TEMP = master.df$TEMPERATURE, LAT = master.df$LAT)

pressures <- pressures[is.na(master.df$PRESS) == F,] # get rid of pesks NAs

one.pressure <- pressures[1,]
one.depth <- marelac::sw_depth(P = (one.pressure$PRES), lat = one.pressure$LAT)
# gives us a depth of -7. 
# however if we make the pressure larger, then the dept goes positive, so not sure this is good
# this doesn't really make sense, and also makes huge assumptions 
# water of salinity 35, and temperature 0 dg C - both wrong!


# let's try a different package

library(oce)

anothyer.depth <- oce::swDepth(pressure = (one.pressure$PRES), 
                               latitude = one.pressure$LAT, eos = "unesco")
# this gives us a more sensible depth of 3m (not a negative depth) - 

oce.depths <- oce::swDepth(pressure = pressures$PRES, latitude = pressures$LAT, 
                           eos = "unesco")

# this doesn't 

max(oce.depths) # reasonable!
min(oce.depths) # zero. hmm. 


# So, let's fill in the missing depths shall we?
practice.vec <- oce::swDepth(pressure = master.df$PRESS[is.na(master.df$PRESS) == F ], latitude = master.df$LAT[is.na(master.df$PRESS) == F ], 
                             eos = "unesco")
save.depths <- master.df$DEPTH
master.df$DEPTH[is.na(master.df$PRESS) == F ] <- oce::swDepth(pressure = master.df$PRESS[is.na(master.df$PRESS) == F ], latitude = master.df$LAT[is.na(master.df$PRESS) == F ], 
                                                              eos = "unesco")

checker <- data.frame(ORIG = save.depths, NEW = master.df$DEPTH) #yes they match




###### OUTDATED/ USELESS CODE ZONE 



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




