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


load("../Data/CTD_master_151021_fixedCruiseID.Rdata")

## ok so let's come up with a timeline for the dates that I've got
# but lets just use the months (not the days) as that may be excessive

ctd.date.normal <- month.day.year(jul = master.df$JDATE, origin = c(month = 1, day = 1, year = -4713)) # convert to something useful
ctd.date.vector <- paste(ctd.date.normal$year, ctd.date.normal$month, ctd.date.normal$day, sep = "-")

# got the ctd.date.normal from a different script, re-create using the chon 
# package and the JDate column if necessary

# ok so now we have a table, let's plot it (show visually)
# start date: 
min(ctd.date.normal$year) # 1979
max(ctd.date.normal$year) # 2017

span = as.numeric(max(ctd.date.normal$year)) - as.numeric(min(ctd.date.normal$year)) 
# 38 years

# let's just histogram the years?
# let's invoke ggplot. time to get this going

library(ggplot2)

dev.off()
plot.new()

plot.data <- as.data.frame(ctd.date.normal)

ggplot(plot.data, aes(x = year)) + geom_histogram(binwidth = 1)  +
  theme_bw() + 
  labs(title = "CTD data points collected over time", x = "Year", y= "# CTD data points") +
  scale_y_continuous(limits = c(0,300000), breaks = seq(0,300000,50000)) + 
  scale_x_continuous(limits =c(1978, 2018), breaks = seq(1978, 2018, 4))


ggsave(filename = "../Figures/CTD_years.png")


#####################################
# Only have model information from 1991, so remove from before this date

master.df$Year <- ctd.date.normal$year
master.df$Month <- ctd.date.normal$month
all.post1991 <- master.df[master.df$Year >= 1991,]


### plot distribution of points 

library(ggplot2)
ggplot(master.df, aes(x = YEAR)) + geom_histogram(binwidth = 1)  +
  theme_bw() + 
  labs(title = "CTD data points collected since 1991", x = "Year", y= "# CTD data points") +
  scale_y_continuous(limits = c(0,300000), breaks = seq(0,300000,50000)) + 
  scale_x_continuous(limits =c(1991, 2018), breaks = seq(1991, 2019, 4))


ggsave(filename = "../Figures/CTD_Since1991.png")



########## investigate how much data we have at different depths

ggplot(master.df, aes(x = DEPTH)) + geom_histogram(binwidth = 1)  +
  theme_bw() + 
  labs(title = "CTD data points collected at different depths", x = "Depth", y= "# CTD data points") 
  #scale_y_continuous(limits = c(0,300000), breaks = seq(0,300000,50000)) + 
  #scale_x_continuous(limits =c(1991, 2018), breaks = seq(1991, 2019, 4))



  
ggsave(filename = "../Figures/CTD_accrossdepths.png")

  
########## investigate how much data we have from different seasons
  
ggplot(master.df, aes(x = MONTH)) + geom_histogram(binwidth = 0.5)  +
    theme_bw() + 
    labs(title = "CTD data points collected at different depths", x = "Month", y= "# CTD data points") +
  scale_y_continuous(limits = c(0,800000), breaks = seq(0,800000,200000)) + 
  scale_x_continuous(limits =c(0,12), breaks = seq(1, 12, 1))
  
ggsave(filename = "../Figures/CTD_accrossmonths.png")


### let's look at the nunber of expeditions that went out over time

expeditions <- data.frame(DATE = master.df$DATE, ID = master.df$CRUISEID)

length(unique(expeditions$ID)) #84
length(is.na(expeditions$ID)) #3518089 - so most of the unique IDs didn't capture

# lets look at how many dips each expedition produced

cruise.id.table <- as.data.frame(table(master.df$CRUISEID))

# let's add a date to that

length(unique(master.df$I))

ggplot(master.df$, aes(x = year)) + geom_histogram(binwidth = 1)  +
  theme_bw() + 
  labs(title = "data from expeditions over time", x = "Year", y= "# CTD data points") +
  scale_y_continuous(limits = c(0,300000), breaks = seq(0,300000,50000)) + 
  scale_x_continuous(limits =c(1978, 2018), breaks = seq(1978, 2018, 4))


# majority of expeditions don't have cruise ID data




# I also would like to look at the spread throughout the year of data (i.e. just take the month)





