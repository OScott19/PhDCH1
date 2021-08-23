##### now we have all of the relevant data extracted (as afar as I know)
# check what I've actually got! 
# do I have all the relevant data for every thing? 

file.vec <- NA
sal.vec <- NA
con.vec <- NA

for (i in 1:7) {
  nametoload <- paste("../Data/LongMasterDF_", i, (".Rdata"), sep = "")
  load(nametoload)
  file.vec <- c(file.vec, master.df$FILE)
  sal.vec <- c(sal.vec, master.df$SALINITY)
  con.vec <- c(con.vec, master.df$CONDUCTIVITY)
}

length(unique(con.vec)) # no conductivity data was captured, so this is all NA

length(unique(sal.vec)) # quite a lot of different salinties, which is good
# howwever a lot of errors did pop up with the salinities, so I might 
# check which files had zero salinity


fs <- data.frame(FILE = file.vec, SAL = sal.vec)

# so, which files didn't have any saility data?


fs.ns <- fs[is.na(fs$SAL),] # tbh this seems like most of them which isn't ideal


fs.ns.u <- unique(fs.ns$FILE) # 2754 files don't have saility data
# as PS001 - so clearly something else going on 


# let's open up the first one and see what salinity is stored as

library(ncdf4) # package for netcdf manipulation
path.to.data <- "../Data/BODC/AllCDTData/"

w.data <- nc_open(file = paste(path.to.data, fs.ns.u[2], sep = ""))
print(w.data)


#   