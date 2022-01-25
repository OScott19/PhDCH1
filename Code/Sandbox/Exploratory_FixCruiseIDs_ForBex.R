# Test script to work out why some of the data from the BODC .nc files
# are giving odd results - particularly the CruiseID data

# Nonny's setup: 
#platform       x86_64-w64-mingw32          
#arch           x86_64                      
#os             mingw32                     
#system         x86_64, mingw32             
#status                                     
#major          4                           
#minor          1.1                         
#year           2021                        
#month          08                          
#day            10                          
#svn rev        80725                       
#language       R                           
#version.string R version 4.1.1 (2021-08-10)
#nickname       Kick Things 



# setup

graphics.off()
rm(list = ls())

library(ncdf4) # package for netcdf manipulation
#update.packages("ncdf4") # just in case 

# Modify as necessary
path.to.data <- "../Data/Data_ForBex/"

# list of files that I've sent you

files <- c("b0072631.nc", "b0072643.nc", "b0072655.nc", "b0072667.nc", 
           "b0072679.nc", "b0072680.nc")

# open the first file, check the unique ID

find.id <- nc_open(file = paste(path.to.data, files[1], sep = ""))
test.id1 <- find.id$id # same as the one in every entry- 196608 (for me) on 14/10/21
nc_close(find.id)

# try again with the second file

find.id2 <- nc_open(file = paste(path.to.data, files[2], sep = ""))
test.id2 <- find.id2$id # identical to above on 14/10/21 
nc_close(find.id2)

# try a third one for luck
find.id3 <- nc_open(file = paste(path.to.data, files[3], sep = ""))
test.id3 <- find.id3$id #ditto
nc_close(find.id3) # also identical 


###########################
###########################
###########################


# trying to extract the variables a slightly different way: 

find.id$var$SDN_CRUISE$id$group_id
find.id2$var$SDN_CRUISE$id$group_id
find.id3$var$SDN_CRUISE$id$group_id

# these also appear to be the same number!



#### the cruise IDs for these files shoud all be different, so something wrong is happening here


# so, call the group_id directly, rather than just 'id'?!
# try in the loop


### test the loop and see where its going wrong & 
## test to see whether it's the "try" function that's the problem: 
# setup:



test1 <- data.frame(I = c(1:5), cruiseID = NA)
test2 <- data.frame(I = c(1:5), cruiseID = NA)

for (i in 1:5) {

  #print("Opening file:")
  #print(r.df$file[i])
  w.data <- nc_open(file = paste(path.to.data, r.df$file[i], sep = ""))
  #ncvar_get(w.data, varid = "group_id")
  # store variables
  
  test1$I[i] <- i
  test2$I[i] <- i
  
  # and now the data
  #print("Data within file:")
  #print(w.data$var$SDN_STATION)
  #print(w.data$var$SDN_CRUISE$id$group_id)
  
  try(test1$cruiseID[i] <- w.data$var$SDN_CRUISE$id$group_id)
  test2$cruiseID[i] <- w.data$var$SDN_CRUISE$id$group_id
  
  nc_close(w.data)

}

#  Nonny's results: if i run this loop, once again i get exactly the same value
# for each of the Cruise IDs
# however, if i don't close the file, something strange happens: 

test3 <- data.frame(I = c(1:5), cruiseID = NA)
test4 <- data.frame(I = c(1:5), cruiseID = NA)

for (i in 1:5) {
   # w.data the "w." prefix here is "working" - ie will change every time
  #print("Opening file:")
  #print(r.df$file[i])
  w.data <- nc_open(file = paste(path.to.data, r.df$file[i], sep = ""))
  #ncvar_get(w.data, varid = "group_id")
  # store variables
  
  test3$I[i] <- i
  test4$I[i] <- i
  
  # and now the data
  #print("Data within file:")
  #print(w.data$var$SDN_STATION)
  #print(w.data$var$SDN_CRUISE$id$group_id)
  
  try(test3$cruiseID[i] <- w.data$var$SDN_CRUISE$id$group_id)
  test4$cruiseID[i] <- w.data$var$SDN_CRUISE$id$group_id

  
}

# THIS time, the value increases by exactly the same amount each time
# so the original value seems like it's been stored, and is getting
# added on each time

check <- c()
check[1:4] <- test3$cruiseID[2:5] - test3$cruiseID[1:4] #


## In conclusion, I'm not sure what's happening here, and I worry that if
# this data isn't being read in properly, then there may also be errors elsewhere
# that are harder to see by eye. 