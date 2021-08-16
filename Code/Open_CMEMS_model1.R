getwd()

rm(list=ls())

# install/ load all the necessary packages

#install.packages("ncdf4")
#install.packages("raster")
#install.packages("rgdal")
#install.packages("sp")
#install.packages("ggplot2")

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

# use "nc_open" to load the file

data <- nc_open(filename = "../Data/CMEMS/Data/copernicus-fulldepth-monthlydata/CMEMS-GLOBAL_001_024-several_vars-2019_2021.nc")

print(data) # this prints the metadata associated with the file 

# saving the print (metadata) to a text file
{ sink("monthlydata_metadata.txt")  
  print(data)  
  }


# this file have 11 variables, excluding dimensional variables

# lat & long are in 1/12 of a degree; depth has 50 z-levels

# 18 global attributes - these provide metadata about the file 

# 4 dimensions: time, latitude, longditude, depth

lon <- ncvar_get(data, "longitude") # 361 entries, which matches the metadata
lat <- ncvar_get(data, "latitude") # 193 entries, also as expected
t <- ncvar_get(data, "time") #  27 - I think this is the number of months!
depth <- ncvar_get(data, "depth") # 50 - which lines up with our z levels

head(lon)

# we can now turn the data into an array (not sure what that means)
# this means we extract the useful information 


# SO, need to pick a variable to test - going to use northward velocity
ndvi.array <- ncvar_get(data, "vo") # vo = northward velocity

dim(ndvi.array) # the dimensions match the dimensions of the long, lat, depth & time above

fillvalue <- ncatt_get(data, "vo", "_FillValue")
# the fill value is -32767. This mean s that if there was missing data, this is the 
# value inthe array



# now we can close our netCDF data 
nc_close(data) # it's still there though. 



### HOUSEKEEPING

# not useful having such a weird value as the missing data value. 
# So, let's replace it with 'NA'. Much clearer. 

ndvi.tidy <- ndvi.array # duplicating it just in case I do something daft (again)
ndvi.tidy[ndvi.tidy == fillvalue$value] <- NA

# let's look at just one month

n.slice <- ndvi.tidy[,,,2] # time is the fourth dimension
dim(n.slice) # all the other dimensions are the same - nice. 

# make into a matrix
na.slice <- as.matrix(n.slice)

# we can now try and save this as a raster

r <- raster(t(na.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), 
               ymx=max(lat) , 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))


# We will need to transpose and flip to orient the data correctly. 
#The best way to figure this out is through trial and error, 
# but remember that most netCDF files record spatial data from the bottom left corner.


r <- flip(r, direction='y')

plot(na.slice)
plot(x = r)

r.m <- as.matrix(r)

plot(r.m) # I honestly don't know what this is showing me


# so I think I'm massively losing somethign in the conversion from array to matrix. 
# let's try again, but I'll drop depth

na2.slice <- n.slice
na2.slice <- na2.slice[,,1] # dropped third dimension - depth 

# and now it turns into a matrix without me transofrming it! great

# lets plot this again 

r2 <- raster(t(na2.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), 
            ymx=max(lat) , 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r2 <- flip(r2, direction='y')

plot(r2)

# OH MY LORD IT WORKED
# This it the northbound water velocity at the surface

# let's try it at DEPTH


na3.slice <- n.slice
na3.slice <- na3.slice[,,40] # dropped third dimension - depth 

# and now it turns into a matrix without me transofrming it! great

# lets plot this again 

r3 <- raster(t(na3.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), 
             ymx=max(lat) , 
             crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r3 <- flip(r3, direction='y')

plot(r3)

# save that damn gorgeous map

writeRaster(r3, "../Figures/NorthwardVelocity_atz40.tif", "GTiff", overwrite=TRUE)

# THIS IS AMAZING!


# next day exploration - going to plot a time series!!!

# we are working with 4D data and the tutorial is 3D
# they use the array, which is 3D, so we still need to slim down out data

#n.slice = 3D, so lets roll with that 

?brick
# multi-layer raster object. Can only onit to a single file


r_brick <- brick(n.slice, xmn=min(lat), xmx=max(lat), ymn=min(lon), 
                 ymx=max(lon), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- flip(t(r_brick), direction='y') # flip so the map is the right orientation

# I have now made a brick!

# we are now going to select a single spot in my map 
# to plot the time series for 

# roughly the middle of the map:
toolik_lon <- -60
toolik_lat <- -60
toolik_series <- extract(r_brick, SpatialPoints(cbind(toolik_lon,toolik_lat)), method='simple')

?extract

# now we do something else 


# i don't understand this bit

toolik_df <- data.frame(depth= c(1:50), NDVI=t(toolik_series))

ggplot(data=toolik_df, aes(x=depth, y=NDVI, group=1)) +
  geom_line() + # make this a line plot
  ggtitle("Lawdy who even knows what this is") +     # Set title
  theme_bw() # use the black and white theme


# working on explaining the graph above. 
# I think it's a profile of the movement over all of the depths, rather than the time 
