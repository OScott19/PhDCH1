graphics.off()
rm(list = ls())
load("pathway.Rdata") # data created in CTD*exploratory

setwd(pathway.to.linux)

# now set actually set the working directory within linux operating system
setwd("PhDCH1/Code/")
path.to.data <- "../Data/BODC/AllCDTData/"

######################
# and load in my DF of all the relevant CTD files

load("../Data/BODC/MoreRelevantFiles.Rdata") # generated in CTD*exploratory
rm(all.df) # not necessary


## load in most recent master df

load("../Data/CTD_master_151023_convertedDepths.Rdata")

######################
### load in necessary packages

library(ncdf4) # package for netcdf manipulation

## JUST CHECKING WHERE THE NAS ARE

length(master.df$TEMP_A[is.na(master.df$TEMP_A) == F]) # quite a few - 1087995
length(master.df$TEMP_P[is.na(master.df$TEMP_P) == F])

temp.data.count <- length(master.df$TEMP_A[is.na(master.df$TEMP_A) == F]) + length(master.df$TEMP_P[is.na(master.df$TEMP_P) == F])
missing.temp <- length(master.df$I) - temp.data.count # just over one million rows

### step one - check to see whether it's becuase there are a couple of missing 
# temperatures in each file 
# do this my getting a list of the IDs of rows WITH temp, and the rows WITHOUT, 
# and comparing the 'i'

no.temp.data <- subset(master.df, is.na(master.df$TEMP_P))
no.temp.data <- subset(no.temp.data, is.na(no.temp.data$TEMP_A))
## the lenght of this data frame should be the same as  missing temp
# it is - great

# now get the unique IDs
no.temp.ids <- unique(no.temp.data$FILE) 
# 967 files have at least one CTD dip with no CTD data


# now we create the opposite df - and see if the IDs overlap

temp.data <- rbind((subset(master.df, is.na(master.df$TEMP_P) == F)), 
               (subset(master.df, is.na(master.df$TEMP_A) == F)))

temp.ids <- unique(temp.data$FILE)
# here we have 2221 unique files
# therefore there will be some overlap as we only have 2982 files in total

overlap.ids <- intersect(no.temp.ids, temp.ids)
# 206 ids overlap - therefore these files have some temperatue data but not all
# how many of these instances exist?

overlap.no.temp <- subset(no.temp.data, no.temp.data$FILE %in% overlap.ids)
# only 26262 entries come from these 206 ID - perhaps an error failure

no.temp.no.overlap <- subset(no.temp.data, no.temp.data$FILE %in% overlap.ids == F)
# ths shuld be roughly 25k shorter than the original overlap - which it is
investigate.ids <- unique(no.temp.no.overlap$FILE) # 761 - which also matches


## so what on earth is going on here? Let's open one up and have a look


w.data <- nc_open(file = paste(path.to.data, investigate.ids[1],  sep =""))
## there is a THIRD temperature variable (my dear goodness)
# let's take a look
print(w.data)

# it's TEMPCC01
# measured in degC
# long name Cal_CTD_Temp
# description Temperature of the water body by CTD and 
# verification against independent measurements



# ok - small loop to see how many of the entrie with no data has this form of temp

nc_close(w.data)

c.temp.files <- c()

for (i in 1:length(investigate.ids)) {
 w.data <- nc_open(file = paste(path.to.data, investigate.ids[i],  sep =""))
 if(length(w.data$var$TEMPCC01) > 0){
   c.temp.files <- c(c.temp.files, investigate.ids[i])
 }
 nc_close(w.data)
}


# there are 300 401 files that have this variable - hooray!


investigate2 <- subset(investigate.ids, investigate.ids %in% c.temp.files == F)
# 360 files remain

# let's try this again

w.data <- nc_open(file = paste(path.to.data, investigate2[1],  sep =""))

print(w.data)
# another new variable

# POTMCV01
# degC
# Potential temperature of the water body by computation using UNESCO 1983 algorithm

# how many files have this name? 
nc_close(w.data)

POTMCV01.ids <- c()

for (i in 1:length(investigate2)) {
  w.data <- nc_open(file = paste(path.to.data, investigate2[i],  sep =""))
  if(length(w.data$var$POTMCV01) > 0){
    POTMCV01.ids <- c(POTMCV01.ids, investigate2[i])
  }
  nc_close(w.data)
}

# 134 have this variable - what else could it be?


investigate3 <- setdiff(investigate2, POTMCV01.ids) ##226 files

# check adds up

length(investigate3) + length(POTMCV01.ids) # YES back on track wahoo


# NEXT STEP

w.data <- nc_open(file = paste(path.to.data, investigate3[1],  sep =""))

print(w.data)

# new variable
# TEMPCU01
# degC
# Temperature of the water body by CTD and NO verification against independent measurements

# losing symapthy fast with whoever made this made pedantic data variable names


nc_close(w.data)

TEMPCU01.ids <- c()

for (i in 1:length(investigate3)) {
  w.data <- nc_open(file = paste(path.to.data, investigate3[i],  sep =""))
  if(length(w.data$var$TEMPCU01) > 0){
    TEMPCU01.ids <- c(TEMPCU01.ids, investigate3[i])
  }
  nc_close(w.data)
}

# 149 variables have this one
# nearly there 


investigate4 <- setdiff(investigate3, TEMPCU01.ids) # 77 SO CLOSE

# check adds up

length(investigate4) + length(TEMPCU01.ids) # YES


################################################
w.data <- nc_open(file = paste(path.to.data, investigate4[1],  sep =""))


print(w.data)

#POTMCV02
#degC
#Potential temperature of the water body 
#by second sensor and computation using UNESCO 1983 algorithm

nc_close(w.data)

POTMCV02.ids <- c()

for (i in 1:length(investigate4)) {
  w.data <- nc_open(file = paste(path.to.data, investigate4[i],  sep =""))
  if(length(w.data$var$POTMCV02) > 0){
    POTMCV02.ids <- c(POTMCV02.ids, investigate4[i])
  }
  nc_close(w.data)
}

##  THANK GOODNESS THAT IS ALL OF THEM!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




# now we just need to add each variable name to the bloody master.df 
# would be silly to have a column for each - instead I'm going to have 
# one column for 'importedTemp' (i.e. this one), and one for the name 
# can import them all and then just convert from there??
# maybe useful for later - all about subsetting


length(unique(investigate2))
length(unique(POTMCV01.ids))

i2P.intersect <- intersect(investigate2, POTMCV01.ids)

length((investigate2 %in% POTMCV01.ids )) # 360 (don't understand this one)
length((POTMCV01.ids %in% investigate2 )) # 134

`%notin%` <- Negate(`%in%`)
length(POTMCV01.ids %notin% investigate2) # 134


length()

length(subset(investigate2, POTMCV01.ids %in% investigate2))




# THESE ARE MY MISSING  TEMPERATURE VARIABLES

# it's TEMPCC01
# measured in degC
# long name Cal_CTD_Temp
# description Temperature of the water body by CTD and 
# verification against independent measurements


# new variable
# TEMPCU01
# degC
# Temperature of the water body by CTD and NO verification against independent measurements
##

# POTMCV01
# degC
# Potential temperature of the water body by computation using UNESCO 1983 algorithm


#POTMCV02
#degC
#Potential temperature of the water body 
#by second sensor and computation using UNESCO 1983 algorithm



###### and missing salinity varibles

#loat PSALST02[MAXZ,INSTANCE]   (Chunking: [151,1])  
#long_name: P_sal_CTD2
#units: Dmnless
# Practical salinity of the water body by CTD (second sensor) and 
#computation using UNESCO 1983 algorithm


# PSALCC01
# dmness
# desc: Practical salinity of the water body by CTD and computation using 
# UNESCO 1983 algorithm and calibration against independent measurements
