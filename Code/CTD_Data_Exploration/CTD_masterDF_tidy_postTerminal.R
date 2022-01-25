graphics.off()
rm(list = ls())
load("pathway.Rdata") # data created in CTD*exploratory

setwd(pathway.to.linux)

# now set actually set the working directory within linux operating system
setwd("PhDCH1/Code/")

######################
# and load in my DF of all the relevant CTD files
## packages

library(oce)

## load in most recent master df

load("../Data/CTD_master_211108_v3_AllConverted.Rdata")


#####

# lets screen out the bad quality data
temp.quals <- as.vector(master.df$TEMP_QUAL)
temp.quals <- temp.quals[!is.na(temp.quals)]
temp.qt <- as.data.frame(table(temp.quals))
# or can just do in a single step
sal.qual <- as.data.frame(table(master.df$SAL_QUAL ))

# 49 - good quality data
# 51 - "probably bad" data
# 56 - interpolated_value

#  don't visualise it as it keeps crashing Rstudio
# instead, let's just screen out all of the poor quality data

master.df <- subset(master.df, master.df$TEMP_QUAL == 49) # drops to 3425258 
master.df <- subset(master.df, master.df$SAL_QUAL == 49) # drops to `3420671`

######### SCREENING TO REMOVE ENTRIES WITHOUT SALINITY AND TEMPERATURE DATA

# how many entries have NA for both salinity & temperature?

na.ts <- subset(master.df, is.na(master.df$TEMP_P))
na.ts <- subset(na.ts, is.na(na.ts$TEMP_A)) # none! I have at least temperature data for all :)
# lets check for salt
na.ts <- subset(master.df, is.na(master.df$SAL_P))
na.ts <- subset(na.ts, is.na(na.ts$SAL_A)) # also for salt. excellent!

row.nos <- as.numeric(rownames(na.ts)) # returns the row numbers - NEEDS TO BE CHANGED IF EITHER OF ABOVE ISN'T ZERO

# remove these from the data base

master.df <- master.df[-row.nos,]

# DIDN'T NEED TO REMOVE ANY ENTRIES. GREAT. 

##########################CONVERTING THE OTHER VARIABLES 


### converting pressure to depth (as required)
#library(oce)
save.depths <- master.df$DEPTH
master.df$DEPTH[is.na(master.df$PRESS) == F ] <- oce::swDepth(pressure = master.df$PRESS[is.na(master.df$PRESS) == F ], 
                                                              latitude = master.df$LAT[is.na(master.df$PRESS) == F ], 
                                                          eos = "unesco")

checker <- data.frame(ORIG = save.depths, NEW = master.df$DEPTH)


#install.packages("gsw")
library(gsw)
library(oce)
#install.packages("seacarb")
library(seacarb)

seacarb::d

### STEP ONE: convert depth to pressure (reverse of above)

length(master.df$PRESS[is.na(master.df$PRESS)]) # 653k 

test.pressure <- d2p(depth = psalsd$DEPTH[1], lat = psalsd$LAT[1] ) # pressure in db!

master.df$PRESS[is.na(master.df$PRESS)] <- d2p(depth = master.df$DEPTH[is.na(master.df$PRESS)],
                                               lat = master.df$LAT[is.na(master.df$PRESS)])


length(master.df$PRESS[is.na(master.df$PRESS)]) # 0 - all been converted

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
load("../Data/CTD_master_211108_v3_AllConverted.Rdata")



ref <- match(min(master.df$JDATE), table = master.df$JDATE)
master.df$DATE[ref]
#1991-12-20


master.df3 <- subset(master.df, master.df$YEAR >= 1993)

ref <- match(min(master.df3$JDATE), table = master.df3$JDATE)
master.df3$DATE[ref]
#1993-1-1"
