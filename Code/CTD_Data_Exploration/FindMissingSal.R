graphics.off()
rm(list = ls())
load("pathway.Rdata") # data created in CTD*exploratory

setwd(pathway.to.linux)

# now set actually set the working directory within linux operating system
setwd("PhDCH1/Code/")
path.to.data <- "../Data/BODC/AllCDTData/"

## load in most recent master df

load("../Data/CTD_master_211023_everydamnTS_PT.Rdata")

######################
### load in necessary packages

library(ncdf4) # package for netcdf manipulation


##### first, let's find the NAs

## JUST CHECKING WHERE THE NAS ARE

length(master.df$SAL_A[is.na(master.df$SAL_A) == F]) # quite a few - 313476
length(master.df$SAL_P[is.na(master.df$SAL_P) == F]) # 495292

SAL.data.count <- length(master.df$SAL_A[is.na(master.df$SAL_A) == F]) + length(master.df$SAL_P[is.na(master.df$SAL_P) == F])
missing.SAL <- length(master.df$I) - SAL.data.count # majority - 2.7million entries

#####

### step one - check to see whether it's becuase there are a couple of missing 
# salinities in each file 
# do this my getting a list of the IDs of rows WITH SAL, and the rows WITHOUT, 
# and comparing the 'i'

no.SAL.data <- subset(master.df, is.na(master.df$SAL_P))
no.SAL.data <- subset(no.SAL.data, is.na(no.SAL.data$SAL_A))
## the lenght of this data frame should be the same as  missing SAL
# it is - great

# now get the unique IDs
no.SAL.ids <- unique(no.SAL.data$FILE) 
# 2230 files have at least one CTD dip with no CTD data


# now we create the opposite df - and see if the IDs overlap

SAL.data <- rbind((subset(master.df, is.na(master.df$SAL_P) == F)), 
                   (subset(master.df, is.na(master.df$SAL_A) == F)))

# same length as SAL.data.count - all good!

SAL.ids <- unique(SAL.data$FILE)
# here we have 863 unique files
# therefore there will be some overlap as we only have 2982 files in total

overlap.ids <- intersect(no.SAL.ids, SAL.ids)
# 11 ids overlap - therefore these files have some salinity data but not all
# how many of these instances exist?

overlap.no.SAL <- subset(no.SAL.data, no.SAL.data$FILE %in% overlap.ids)
# just 1079 entries come from these 111 ID - perhaps an CTD failure?

no.SAL.no.overlap <- subset(no.SAL.data, no.SAL.data$FILE %in% overlap.ids == F)
# ths shuld be roughly 1k shorter than the original overlap - which it is

########### FINDING THE MISSING SAL VARIABLE NAMES


investigate1 <- unique(no.SAL.no.overlap$FILE) # 2119 - which also matches


# i picked up a few new sal variable names whilst going through the temps
# lets try these first and see how many variables are left


files1 <- c()

for (i in 1:length(investigate1)) {
  w.data <- nc_open(file = paste(path.to.data, investigate1[i],  sep =""))
  # two variable names to work with - both practical salinity 
  if(length(w.data$var$PSALST02) > 0){
    files1 <- c(files1, investigate1[i])
  }
  if(length(w.data$var$PSALCC01) > 0){
    files1 <- c(files1, investigate1[i])
  }
  nc_close(w.data)
}
# 670!
# let's check they're all unique
length(unique(files1)) # yep! wonderful

# now, remove the files that we've found the sal reading for and 
# look for the other salinity variables

investigate2 <- setdiff(investigate1, files1) ##226 files

w.data <- nc_open(file = paste(path.to.data, investigate2[1],  sep =""))
print(w.data)

#PSALST01
#Dmnless
#Practical salinity of the 
#water body by CTD and computation using UNESCO 1983 algorithm

# how many have this one? 

files2 <- c()
check <- c()

for (i in 1:length(investigate2)) {
  w.data <- nc_open(file = paste(path.to.data, investigate2[i],  sep =""))
  # two variable names to work with - both practical salinity 
  if(length(w.data$var$PSALST01) > 0){
    files2 <- c(files2, investigate2[i])
  }
  # haven't observed the below (hence the check), but curious!
  if(length(w.data$var$PSALCC02) > 0){
    files2 <- c(files2, investigate2[i])
    c <- c(c, "yes")
  }
  
  nc_close(w.data)
}

# so, the second variable wasn't real, but 1357 in the first one! hooray!
length(unique(files2)) # and they're all unique. Yee haw

investigate3 <- setdiff(investigate2, files2) ## only 92 left. amazing. 

w.data <- nc_open(file = paste(path.to.data, investigate3[1],  sep =""))
print(w.data)

#PSALCU01
#Dmnless
#Practical salinity of the water body by CTD and computation using 
# UNESCO 1983 algorithm and NO calibration against independent measurementsm

# how many have this one? 

files3 <- c()

for (i in 1:length(investigate3)) {
  w.data <- nc_open(file = paste(path.to.data, investigate3[i],  sep =""))
  # two variable names to work with - both practical salinity 
  if(length(w.data$var$PSALCU01) > 0){
    files3 <- c(files3, investigate3[i])
  }
  # haven't observed the below (hence the check), but curious!

  nc_close(w.data)
}
# 89. THREE SNEAKY FILES TO GO 

investigate4 <- setdiff(investigate3, files3) ## only   3 left. amazing. 

w.data <- nc_open(file = paste(path.to.data, investigate4[1],  sep =""))
print(w.data)

#PSALCU02
#Dmnless
#Practical salinity of the water body by CTD (second sensor) and 
#computation using UNESCO 1983 algorithm and NO calibration against independent measurements

files4 <- c()

for (i in 1:length(investigate4)) {
  w.data <- nc_open(file = paste(path.to.data, investigate4[i],  sep =""))
  # two variable names to work with - both practical salinity 
  if(length(w.data$var$PSALCU02) > 0){
    files4 <- c(files4, investigate4[i])
  }}


## that's all of them sorted out!

#so, we have FIVE new sailinity variable names


# They are all ractical salinity: 

#PSALST01
#Dmnless
#Practical salinity of the 
#water body by CTD and computation using UNESCO 1983 algorithm

#PSALST02
#units: Dmnless
# Practical salinity of the water body by CTD (second sensor) and 
#computation using UNESCO 1983 algorithm


# PSALCC01
# dmness
# desc: Practical salinity of the water body by CTD and computation using 
# UNESCO 1983 algorithm and calibration against independent measurements

#PSALCU02
#Dmnless
#Practical salinity of the water body by CTD (second sensor) and 
#computation using UNESCO 1983 algorithm and NO calibration against independent measurements

#PSALCU01
#Dmnless
#Practical salinity of the water body by CTD and computation using 
# UNESCO 1983 algorithm and NO calibration against independent measurementsm

