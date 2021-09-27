graphics.off()
rm(list = ls())
load("pathway.Rdata") # data created in CTD*exploratory

setwd(pathway.to.linux)

# now set actually set the working directory within linux operating system
setwd("PhDCH1/Code/")


######################
### load in necessary packages

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(chron) # converts julian dates (important!)

# load in my master.df file


load("../Data/CTD_master_tempsalSplit.Rdata")

## ok so let's come up with a timeline for the dates that I've got
# but lets just use the months (not the days) as that may be excessive

ctd.date.monthyear <- paste(ctd.date.normal$year, ctd.date.normal$month, sep = "-")
date.table <- as.data.frame(table(ctd.date.monthyear))

# ok so now we have a table, let's plot it (show visually)
# start date: 
min(ctd.date.normal$year) # 1979
max(ctd.date.normal$year) # 2017

span = as.numeric(max(ctd.date.normal$year)) - as.numeric(min(ctd.date.normal$year)) 
# 38 years

months <- rep(c(1:12), 38)
years <- sort(rep(c(1979:2019), 12))
dates.plot <- paste(years, months, sep = "-")

# create the data frame

dates.for.plotting = data.frame(DATE = dates.plot,FREQ = NA)

# and now we match

for (i in 1:length(date.table)) {
  ref <- match(date.table$ctd.date.monthyear[i], dates.for.plotting$DATE)
  dates.for.plotting$FREQ[ref] <- date.table$Freq[i]
}


## ok we're struggling here

# let's just histogram the years?
graphics.off()
plot.new()
hist(ctd.date.normal$year, 
     title(main = "Number of CTD dips collected over time", 
           xlab = "year", 
           ylab = "number of dips"))



# we really should look at the number of expeditions too 

hist(as.numeric(ctd.date.monthyear))
barplot(height = dates.for.plotting$FREQ, x = dates.for.plotting$DATE)
plot(x = dates.for.plotting$DATE, y = dates.for.plotting$FREQ)