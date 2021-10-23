graphics.off()
rm(list = ls())
load("pathway.Rdata") # data created in CTD*exploratory

setwd(pathway.to.linux)

# now set actually set the working directory within linux operating system
setwd("PhDCH1/Code/")
path.to.data <- "../Data/BODC/AllCDTData/"

## load in most recent master df

load("../Data/CTD_master_211023_everydamnTS_PT.Rdata")

###

# how many entries have NA for both salinity & temperature?

na.ts <- subset(master.df, is.na(master.df$TEMP_P))
na.ts <- subset(na.ts, is.na(na.ts$TEMP_A)) # 87548
na.ts <- subset(na.ts, is.na(na.ts$SAL_A)) # doesn't change
na.ts <- subset(na.ts, is.na(na.ts$SAL_P)) # drops marginly to 87462


row.nos <- as.numeric(rownames(na.ts)) # returns the row numbers 

# remove these from the data base

master.df <- master.df[-row.nos,]

# from 3518089 to 3430627


save(master.df, file = "../Data/CTD_master_211023_NArm.Rdata")

######################################
########################################
# NEXT - WE REMOVE OUT-OF-DATE DATA

library(chron)

ctd.date.normal <- month.day.year(jul = master.df$JDATE, origin = c(month = 1, day = 1, year = -4713)) # convert to something useful
ctd.date.vector <- paste(ctd.date.normal$year, ctd.date.normal$month, ctd.date.normal$day, sep = "-")

master.df$YEAR <- ctd.date.normal$year
master.df$MONTH <- ctd.date.normal$month



master.df <- subset(master.df, master.df$YEAR >= 1991 )

##### 
