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


## load in most recent master df

load("../Data/CTD_master_151023_convertedDepths.Rdata")
path.to.data <- "../Data/BODC/AllCDTData/"

######################
### load in necessary packages

library(ncdf4) # package for netcdf manipulation
library(chron) # converts julian dates (important!)



## JUST CHECKING WHERE THE NAS ARE
length(master.df$I[is.na(master.df$I)]) # None!
length(master.df$DATE[is.na(master.df$DATE)]) # also none
length(master.df$DATE[is.na(master.df$DEPTH)]) # quite a few 
length(master.df$JULIANINFO[is.na(master.df$JULIANINFO)])
length(master.df$TEMP_A[is.na(master.df$TEMP_A) == F]) # quite a few - 1087995
length(master.df$TEMP_P[is.na(master.df$TEMP_P) == F])
length(master.df$TEMPERATURE_UNIT[is.na(master.df$TEMPERATURE_UNIT)]) # more than before - weird! - 1413952
length(master.df$LAT[is.na(master.df$LAT)]) # back to None! 
length(master.df$LONG[is.na(master.df$LONG)]) # back to None phew 
length(master.df$CRUISEID[is.na(master.df$CRUISEID)]) # back to None phew 
length(master.df$FILE[is.na(master.df$FILE)]) # back to None phew 
length(master.df$SAL_P[is.na(master.df$SAL_P) == F]) # 3022797 - only 0.5mil
length(master.df$SAL_A[is.na(master.df$SAL_A) == F]) # 3204613 - and only 0.3mil
### could the others have conductivity measurements instead?
length(master.df$SALINITY_UNIT[is.na(master.df$SALINITY_UNIT)]) # 2708242
### so it turns out that we have quite a lot more missing temperature data than I thought
# and we also have quite a lot of missing salinity data. not good!

####################################
#####################################

# ok need to convert from practical to in-situ salinity before I can
# convert the pressure (as per the equation requirement below)


# let's only use the 

actual.sals <- subset(master.df$SAL_A, is.na(master.df$SAL_A) == F)

master.df$LONG360 <- master.df$LONG + 360 # needed for marelac to run! 

### this didn't work - I cancelled it after 30/40 minutes and nothing came out
## try with a single value, and if that doesn't work, then re-think


# CONVERTING FROM PRACTICAL TO ACTUAL
practical.sals <- subset(master.df, is.na(master.df$SAL_P) == F)
# also need pressure data


practical.sals <- subset(practical.sals, is.na(practical.sals$PRESS) == F)
#494k results

p2as <- marelac::convert_PStoAS(S = practical.sals$SAL_P[1], p = practical.sals$PRESS[1]/10, 
                                lat = practical.sals$LAT, 
                                lon = practical.sals$LONG360, 
                                Ocean = "Southern")
??marelac::convert_PStoAS
marelac::conver
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



