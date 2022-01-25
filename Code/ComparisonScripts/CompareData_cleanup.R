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
library(gridExtra)


load("pathway.Rdata") # data created in CTD*exploratory

setwd(pathway.to.linux)

# now set actually set the working directory within linux operating system
setwd("PhDCH1/Code/")
path.to.data <- "../Data/BODC/AllCDTData/"


###### now, let's load in our matched dataset

load("../Data/CMEMS_AllFixed_matched.Rdata")

# remove rows that are ALL NAs
ind <- apply(all.model.data, 1, function(x) all(is.na(x)))
clean.matched  <- all.model.data[ !ind, ]

all.model.data <- clean.matched

length(unique(all.model.data$UNIQ)) * 50
#all.model.data <- all.model.data[-c(1,2),]

length(unique(all.model.data$UNIQ)) # 2270 - a good number

load("../Data/CTD_master_211108_v3_AllConverted.Rdata")

# how many unique dates are there in the overlapping ctd data?

master.df$UNIQ <- paste(master.df$LAT, master.df$LONG, master.df$DATE, sep = "")
ctd.overlap <- master.df[master.df$YEAR >= 1993,]

length(unique(ctd.overlap$UNIQ)) # 2270!!! they match! marvellous 



# let's check max/min lats & longs

model <- nc_open(file = "D:/CMEMS_001_030_MONTHLY/CMEMS-GLOBAL_001_030-bottomT_thetao_so-200001.nc")
lats <- ncvar_get(model, "latitude")
min(lats) #-67
max (lats)#-50
longs <- ncvar_get(model, "longitude")
min(longs)#-70
max(longs)#-21

max(ctd.overlap$LAT) #-50
min(ctd.overlap$LAT) # -66.8
min(ctd.overlap$LONG) # -69.9
max(ctd.overlap$LONG) #-21.23

# ALL SEEMS TO BE MATCHING & IN ORDER. GREAT. 

# LET'S MOVE ON TO SENSE-CHECK THE MATCHING PROCESS

all.model.data$diffA <- abs(all.model.data$modDEPTH - all.model.data$ctdDEPTH) # want absolute for calcs
all.model.data$diffP <- all.model.data$diffA/ all.model.data$modDEPTH 
max(all.model.data$diffP, na.rm = T) # 100 - that's pretty big 

library(ggplot2)

# let's look at matching gaps accross the depths

qplot(x = all.model.data$modDEPTH, y = all.model.data$diffA)
qplot(x = all.model.data$modDEPTH, y = all.model.data$ctdDEPTH)
all.model.data$COL <- "1"
all.model.data$COL[all.model.data$diffP > 0.5] <- "2"
all.model.data$COL[which (all.model.data$modDEPTH > 2000 & all.model.data$diffP > 0.25) ] <- "2"
all.model.data$COL[which (all.model.data$modDEPTH > 4000 & all.model.data$diffP > 0.1) ] <- "2"

accuracy <- as.data.frame(table(all.model.data$COL))

ggplot( data = all.model.data) + 
  geom_point(aes(x = modDEPTH, y = ctdDEPTH, color = COL))


ggsave(file = "../Figures/MatchedDataAccuracy_colourful.png")

### THINGS THAT NEED TO BE DEFENDED: HOW I SELECTED THESE CUT-OFF POINTS


hist(all.model.data$diffP)


## so clearly some of the matches aren't great


## let's slim it down, so ctd can only be matched if the difference is <50% of the value of the matched depth
# r = refined
r.overlap <- all.model.data[all.model.data$COL == "1",] # from 115k to 88k - lost c.24%, not toooo bad

#let's replot these

plot(x = r.overlap$modDEPTH, y = r.overlap$diffA)
plot(x = r.overlap$modDEPTH, y = r.overlap$diffP)
qplot(x = r.overlap$modDEPTH, y = r.overlap$ctdDEPTH)

# gray/black plot showing what was removed during cleaning 

## SO MUCH TIGHTER!! 

save(r.overlap, file = "../Data/RefinedOverlapData_2022.Rdata")

hist(r.overlap$ctdDEPTH)
hist(r.overlap$modDEPTH)

# oh good,they look similar!!









