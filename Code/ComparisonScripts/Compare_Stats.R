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

## and for plotting

library(grid)
library(ggpubr)
library(patchwork)
library("Metrics")
#install.packages("patchwork")
#install.packages("ggpubr")


load("pathway.Rdata") # data created in CTD*exploratory

setwd(pathway.to.linux)

# now set actually set the working directory within linux operating system
setwd("PhDCH1/Code/")
path.to.data <- "../Data/BODC/AllCDTData/"


### load in my data set
load("../Data/RefinedOverlapData_2022.Rdata")



# NEXT STEPS 
# do some R squared stuff to compare accross: time, depths & whole data frame
# let's also plot salinity vs temperature for all of this matched data
# ALSO LETS MAKE A MAP OF THESE LOCATIONS! 


rmse <- function(a,b) {
  sqrt(mean((a - b)**2))
}

#rms.package <- rmse(data$actual, data$predicted)
r.overlap <- na.omit(r.overlap) # 88k to 83k, removes 5k rows that contain NAs. 

rms.sal <- rmse(r.overlap$ctdSAL,
                r.overlap$SAL.m1)

rms.temp <- rmse(r.overlap$ctdTEMP,
                 r.overlap$TEMP.m1)

rms.depth.sal <- c()
rms.depth.temp <- c()
depths <- sort(unique(r.overlap$modDEPTH))

for (d in 1:length(depths)) {
  temp  <- subset (r.overlap, r.overlap$modDEPTH == depths[d])
  rms.depth.sal<- c(rms.depth.sal, rmse(temp$SAL.m1, temp$ctdSAL))
  rms.depth.temp <-c(rms.depth.temp, rmse(temp$TEMP.m1, temp$ctdTEMP))
}

rms_depths <- grid.arrange(qplot(x = depths, y = rms.depth.sal), qplot(x = depths, y = rms.depth.temp)
                           , nrow = 2)

ggsave(rms_depths, filename = "../Figures/RMS_acrossDepths_2022.png")

# repeat the code above for years

years <- sort(as.numeric(unique(substring(r.overlap$DATE, 1, 4))))
r.overlap$YEARS <- as.numeric(substring(r.overlap$DATE, 1, 4))
rms.yearly.sal <- c()
rms.yeary.temp <- c()

for (y in 1:length(years)) {
  temp  <- subset (r.overlap, r.overlap$YEARS == years[y])
  rms.yearly.sal<- c(rms.yearly.sal, rmse(temp$SAL.m1, temp$ctdSAL))
  rms.yeary.temp <-c(rms.yeary.temp, rmse(temp$TEMP.m1, temp$ctdTEMP))
}

rms_years <- grid.arrange(qplot(x = years, y = rms.yearly.sal), 
                          qplot(x = years, y = rms.yeary.temp),
                          nrow = 2)

ggsave(rms_years, filename = "../Figures/RMS_acrossYears_2022.png")

## now - how can I add in some stats to this to work out whether it changes over time with 
# statistical significance? 

## no pattern over time 
# but definiteiy a pattern over depths

#######################
########################
####################


