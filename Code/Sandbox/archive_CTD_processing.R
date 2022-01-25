graphics.off()
rm(list = ls())
load("pathway.Rdata") # data created in CTD*exploratory

setwd(pathway.to.linux)

# now set actually set the working directory within linux operating system
setwd("PhDCH1/Code/")
path.to.data <- "../Data/BODC/AllCDTData/"

## load in most recent master df

load("../Data/CTD_master_211105_goodqual.Rdata")

###

# how many entries have NA for both salinity & temperature?

na.ts <- subset(master.df, is.na(master.df$TEMP_P))
na.ts <- subset(na.ts, is.na(na.ts$TEMP_A)) # none! I have at least temperature data for all :)
# lets check for salt
na.ts <- subset(master.df, is.na(master.df$SAL_P))
na.ts <- subset(na.ts, is.na(na.ts$SAL_A)) # also for salt. excellent!


row.nos <- as.numeric(rownames(na.ts)) # returns the row numbers 

# remove these from the data base

master.df <- master.df[-row.nos,]

# from 3518089 to 3430627


#### now convert into useful forms 

##### 




#install.packages("gsw")
library(gsw)


# therefore, need non-zero numbers for pressure & sailinity
psals <- master.df[!is.na(master.df$SAL_P),]

# need to have actual salinity before we can convert the temperatures
# so need: practical salinity, longitude, latitude & pressure
test <- gsw::gsw_SA_from_SP(SP = psals$SAL_P[1], latitude = psals$LAT[1], longitude = psals$LONG[1],
                            p = psals$PRESS[1])

# ok but how many psal entries also have pressure data?

psalsp <- psals[!is.na(psals$PRESS),] # most of them - ok, not too bad :) 
psalsd <- psals[is.na(psals$PRESS),]

# find the missing pressure? 
# ok so let's fill in the missing pressure using the d2p function

library(oce)
#install.packages("seacarb")
library(seacarb)

test.pressure <- d2p(depth = psalsd$DEPTH[1], lat = psalsd$LAT[1] ) # pressure in db!

### ok.... so let's create a pressure for all the entries without one

length(master.df$PRESS[is.na(master.df$PRESS)]) # 653k 

master.df$PRESS[is.na(master.df$PRESS)] <- d2p(depth = master.df$DEPTH[is.na(master.df$PRESS)],
                                               lat = master.df$LAT[is.na(master.df$PRESS)])


length(master.df$PRESS[is.na(master.df$PRESS)]) # 0 - all been convered

###### AFTER PRESSURE, NOW WE CAN CONVERT SALINITY

## converting practical salinity to absolute salinity 

test <- gsw::gsw_SA_from_SP(SP = psals$SAL_P[1], latitude = psals$LAT[1], longitude = psals$LONG[1],
                            p = psals$PRESS[1])

length(master.df$SAL_A[is.na(master.df$SAL_A)]) # 3 mil entries

master.df$SAL_A[is.na(master.df$SAL_A)] <- gsw::gsw_SA_from_SP(SP = master.df$SAL_P[is.na(master.df$SAL_A)], 
                                                               latitude = master.df$LAT[is.na(master.df$SAL_A)], 
                                                               longitude = master.df$LONG[is.na(master.df$SAL_A)],
                                                               p = master.df$PRESS[is.na(master.df$SAL_A)])


length(master.df$SAL_A[is.na(master.df$SAL_A)]) # none


######################## and finally.... we can convert the temperature!

length(master.df$TEMP_A[!is.na(master.df$TEMP_A)])## 2.5 mil temps
# double check this against the temp_P to make sure it makes sense

length(master.df$TEMP_P[is.na(master.df$TEMP_P)])

# in following function, use: t =in-situ temperature, SA = absolute salinity, 
# p = sea pressure (dbar), p_ref = reference pressure

single.temp <- gsw::gsw_pt_from_t(SA = master.df$SAL_A[1], t = master.df$TEMP_A[1], p = master.df$PRESS[1])

all.temps <- gsw::gsw_pt_from_t(SA = master.df$SAL_A[!is.na(master.df$TEMP_A)],
                                t = master.df$TEMP_A[!is.na(master.df$TEMP_A)],
                                p = master.df$PRESS[!is.na(master.df$TEMP_A)])

max(all.temps, na.rm = T) # 16
min(all.temps, na.rm = T) # -1.8
# yep, looking reasonable! 

## now add in to the main data.frame

master.df$TEMP_P[is.na(master.df$TEMP_P)] <- all.temps # in the right place


# and save it down

save(master.df, file = "../Data/CTD_master_211108_v3_AllConverted.Rdata")


