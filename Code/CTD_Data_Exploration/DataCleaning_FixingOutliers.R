graphics.off()
rm(list = ls())
load("pathway.Rdata") # data created in CTD*exploratory

setwd(pathway.to.linux)

# now set actually set the working directory within linux operating system
setwd("PhDCH1/Code/")
path.to.data <- "../Data/BODC/AllCDTData/"

## load in most recent master df

load("../Data/CTD_master_211024_post1991.Rdata")


### RIGHT 
# THIS SCRIPT IS HERE TO IDENTIFY THE FILES THAT HAVE STRANGE TEMPERATURES

unique(ist$PRESSTXT) # some are in decibar, and some have NA - indicating no pressure

# let's remove the entries with no pressure data
ist$PRESS_BAR <- ist$PRESS/10
ist.nona <- subset(ist, is.na(ist$PRESS) == F)
unique(ist.nona$PRESSTXT) # excellent, only dbar things left now

some.temps <- gsw::gsw_pt_from_t(SA = ist.nona$SAL_P, t = ist.nona$TEMP_A, p = ist.nona$PRESS_BAR)

### let's look at the boundaries of these then

min(some.temps, na.rm = T) #[1] -49.41303

max(some.temps, na.rm = T) # 50.37707

### ok so clearly, something else is up
# let's check for NAs in the salinity 

ist.3 <- subset(ist.nona, is.na(ist.nona$SAL_P) == F)
fewer.temps <- gsw::gsw_pt_from_t(SA = ist.3$SAL_P, t = ist.3$TEMP_A, p = ist.3$PRESS_BAR)

min(fewer.temps, na.rm = T) # still far too low
max(fewer.temps, na.rm = T) # ditto

max(ist.3$TEMP_A) # THIS IS WHERE THE ERROR IS!
# UGHH

# let's have a look at the distribution of these temperatures and see how many weird ones we have

library(ggplot2)


ggplot(master.df, aes(x = TEMP_A)) + geom_histogram(binwidth = 1)  +
  theme_bw() 


# let's make a table as the histogram isn't that helful 

rounded.temp <- round(master.df$TEMP_A) 

temps.table <- as.data.frame(table(rounded.temp))


# ok - so there are quite a few funky temps - both low and high
# let's have a look at the files that these came from and perhaps exclude if needbe

## high temps

high.temps <- subset(master.df, master.df$TEMP_A >= 10)

# files

high.files <- unique(high.temps$FILE)
# just two files - interesting

library(ncdf4)
w.data <- nc_open(file = paste(path.to.data, high.files[1], sep = ""))

print(w.data)
# tempCC01 - temp of water by CTD & vertification against independent measurements

tc <- data.frame(TEMP = ncvar_get(w.data, "TEMPCC01"))
tc$CONTROL <-  ncvar_get(w.data, "TEMPCC01_SEADATANET_QC")
# getting the data quality flags
# 48 = no quality control
 """
flag_values: 48 no_quality_control
flag_values: 49 good_value 
flag_values: 50 probably_good_value 
flag_values: 51 probably_bad_value 
flag_values: 52 bad_value 
flag_values: 53changed_value 
flag_values: 54 value_below_detection 
flag_values: 55 value_in_excess 
flag_values: 56 interpolated_value 
flag_values: 57 missing_value 
flag_values: 65 value_phenomenon_uncertain

 """
 
## let's plot the temperature vs the values
 
ggplot(tc, aes(y = TEMP, x = CONTROL)) + geom_point()  +
   theme_bw() 

 
# so - the strangely high temperatuere is flagged as 51 - 'probably bad'
 # all others fall in a sensible raange
 
 # let's check the the other file with high numbers
 
 w.data <- nc_open(file = paste(path.to.data, high.files[2], sep = ""))
 
 print(w.data)
 # tempCC01 - temp of water by CTD & vertification against independent measurements
 
 tc2 <- data.frame(TEMP = ncvar_get(w.data, "TEMPST01"))
 tc2$CONTROL <-  ncvar_get(w.data, "TEMPST01_SEADATANET_QC")
 
 ggplot(tc2, aes(y = TEMP, x = CONTROL)) + geom_point()  +
   theme_bw() 
 
 # one high temperature taht is glagged as ok, but extremely high & low flagged as
 # 'probably bad'
 
 
# now let's try this with the low tempeatures
 
 
low.temps <- subset(master.df, master.df$TEMP_A < -2)
low.files <- unique(low.temps$FILE) # only three - and one is an overlap

# let's double check this data too!

w.data <- nc_open(file = paste(path.to.data, low.files[1], sep = ""))

print(w.data)
# tempCC01 - temp of water by CTD & vertification against independent measurements

tc3 <- data.frame(TEMP = ncvar_get(w.data, "TEMPCC01"))
tc3$CONTROL <-  ncvar_get(w.data, "TEMPCC01_SEADATANET_QC")

ggplot(tc2, aes(y = TEMP, x = CONTROL)) + geom_point()  +
  theme_bw() 
### 51 strikes again! 
 
## and the last file
w.data <- nc_open(file = paste(path.to.data, low.files[2], sep = ""))

print(w.data)
# tempCC01 - temp of water by CTD & vertification against independent measurements

tc3 <- data.frame(TEMP = ncvar_get(w.data, "TEMPCC01"))
tc3$CONTROL <-  ncvar_get(w.data, "TEMPCC01_SEADATANET_QC")

ggplot(tc2, aes(y = TEMP, x = CONTROL)) + geom_point()  +
  theme_bw() 
 # lots of the freaky data is all labelled as bad, which is excellent. 


# in the short term, I could just screen out these files
# but in the long term, I can re-read in all of the data and include the 
# data quality flags, and then first thing to do would be to screen out any 
# of the data that is flagged as bad!


# so - I think we've got to the bottom of things
# the original data was a bit funky, and it turns out it was poor quality. 

 
# most of the 'good' values are around 0
 # one high value is 'probably bad'. 
 # therefore, let's remove it 
 # should I go through and remove all of the 'probably bad' data??
 # ASK ADVISORS THIS
 
 