graphics.off()
rm(list = ls())


library(ncdf4) # package for netcdf manipulation

load("pathway.Rdata") # data created in CTD*exploratory

setwd(pathway.to.linux)

# now set actually set the working directory within linux operating system
setwd("PhDCH1/Code/")

######################
# and load in my DF of all the relevant CTD files

load("../Data/BODC/MoreRelevantFiles.Rdata") # generated in CTD*exploratory
rm(all.df) # not necessary


# load the current version of the master_df

load("../Data/CTD_master_tempsalSplit.Rdata")

###################
##################
# FINDING MISSING CRUISE IDS

# pick a file that doesn't have a cruise ID, using master.df or 'expeditions'
# nb expeditions was created in CTD_background script 

master.df <- master.df[-1,]
c.ids <- unique(master.df$CRUISEID) #every cruise has the same cruise ID. somethign is going wrong! 
# they're all down as 65536

#unique files
files <- unique(master.df$FILE)

path.to.data <- "../Data/BODC/AllCDTData/"



# open the first file, check the unique ID
find.id <- nc_open(file = paste(path.to.data, files[1], sep = ""))
test.id11 <- ncvar_get(find.id,"SDN_CRUISE")
test.id1 <- find.id$id # same as the one in every entry- 65536
nc_close(find.id)

# try again with the second file

find.id2 <- nc_open(file = paste(path.to.data, files[2], sep = ""))
test.id2 <- find.id2$id #131072 this one is different!! 
nc_close(find.id2)

# try a third one for luck
find.id3 <- nc_open(file = paste(path.to.data, files[21], sep = ""))
test.id3 <- find.id3$id #131072 this one is different!! 
nc_close(find.id3) # this one is also different. Therefore, in the master algo something is going wrong


find.id4 <- nc_open(file = paste(path.to.data, files[22], sep = ""))
test.id4 <- find.id4$id #131072 this one is different!! 
nc_close(find.id4)

find.id5 <- nc_open(file = paste(path.to.data, files[23], sep = ""))
test.id5 <- find.id5$id #131072 this one is different!! 
nc_close(find.id4)

find.id6 <- nc_open(file = paste(path.to.data, files[1], sep = ""))
test.id6 <- find.id6$id #131072 this one is different!! 
nc_close(find.id4)

store <- c(test.id1, test.id2, test.id3, test.id4, test.id5, test.id6)

## ALL OF THE ABOVE CHANGE DRAMATICALLY EVERY TIME - THIS DOES NOT SEEM TO WORK
## GRRR



###########################
###########################
###########################


## THIS SEEMS TO WORK!!!!!

find.id$var$SDN_CRUISE$id$group_id
find.id2$var$SDN_CRUISE$id$group_id
find.id3$var$SDN_CRUISE$id$group_id
find.id4$var$SDN_CRUISE$id$group_id

# so, call the group_id directly, rather than just 'id'?!
# try in the loop


### test the loop and see where its going wrong
# setup:

path.to.data <- "../Data/BODC/AllCDTData/"
load("../Data/BODC/MoreRelevantFiles.Rdata") # generated in CTD*exploratory
rm(all.df) # not necessary

# go: 

test.master.df <- data.frame(I = NA, DATE = NA, JULIANINFO = NA, 
                             LAT = NA, LONG = NA, CRUISEID = NA, MAXDEPTH = NA, 
                             FILE = NA)

holding.df <-  test.master.df

# first one
i <- 111
# second one
i <- 98

i <- 2


## test to see whether it's the "try" function that's the problem: 

test1 <- data.frame(I = c(1:500), cruiseID = NA)
test2 <- data.frame(I = c(1:500), cruiseID = NA)

for (j in 100:599) {
  # to ensure we're not carrying over data, we resset the holding data frame to be empty each time
  # we then open the .nc file (one at a time)
  # w.data the "w." prefix here is "working" - ie will change every time
  print(j)
  print("Opening file:")
  print(r.df$file[j])
  w.data <- nc_open(file = paste(path.to.data, r.df$file[j], sep = ""))
  #ncvar_get(w.data, varid = "group_id")
  # store variables
  
  i <- j-99
  test1$I[i] <- i
  test2$I[i] <- i
  
  # and now the data
  print("Data within file:")
  print(w.data$var$SND_CRUISE)
  #print(w.data$var$SDN_STATION)
  #print(w.data$var$SDN_CRUISE$id$group_id)
  
  try(test1$cruiseID[i] <- ncvar_get(w.data,"SDN_CRUISE"))
  test2$cruiseID[i] <- ncvar_get(w.data,"SDN_CRUISE")
  
  #nc_close(w.data)

}

check[1:4] <- test1$cruiseID[2:5] - test1$cruiseID[1:4] # all the same
# SO THERE IS A PROBLEM WITH THIS 


## after two iterations
unique(test.master.df$CRUISEID)
run1 <- unique(test.master.df$CRUISEID)
run2 <-unique(test.master.df$CRUISEID)
run3 <- unique(test.master.df$CRUISEID)


unique(test1$cruiseID)





##### ONCE THIS SCRIPT WORKS, ADJUST THE MASTERDF script & change name to 'finished'