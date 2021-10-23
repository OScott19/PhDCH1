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

load("../Data/CTD_master_211023_everydamnTS_T.Rdata")


### converting pressure to depth (as required)

save.depths <- master.df$DEPTH
master.df$DEPTH[is.na(master.df$PRESS) == F ] <- oce::swDepth(pressure = master.df$PRESS[is.na(master.df$PRESS) == F ], 
                                                              latitude = master.df$LAT[is.na(master.df$PRESS) == F ], 
                                                              eos = "unesco")

checker <- data.frame(ORIG = save.depths, NEW = master.df$DEPTH)


# will then need to save the final version of the master df

save(master.df, file = "../Data/CTD_master_211023_everydamnTS_PT.Rdata")
