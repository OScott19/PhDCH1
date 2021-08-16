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

# also let's load in some other packages?
#install.packages("lubridate")
library(lubridate) # used to help work with dates
#install.packages("oce")
library(oce) # this is for reading and processing CTD data
library(sf) # for mapping 

# use "nc_open" to load the file

# ok shall we try and read in one of the CTD data?

# first let's load in the list of files

all <-  read.csv("../Data/BODC/AllCDTData/listoffiles.txt")

all.df <- data.frame(file = all$X1009513.html, end = NA, relevant = NA)

end <- strsplit(all.df$file, split = ".", fixed = T)

end[[1]][2] # end is now the second part of a list

for (i in 1:length(end)) {
  all.df$end[i] <- end[[i]][2]
} 


all.df$relevant <- F
all.df$relevant[all.df$end == "nc"] <- T

save(all.df, file = "../Data/BODC/RelevantFiles.Rdata") # saved so I don't need to do this again


only.nc <- all.df[all.df$end == "nc",]

# now I have a list of all of the files that I would like to use
# let's open the first one

path.to.data <- "../Data/BODC/AllCDTData/"

data <- nc_open(file = paste(path.to.data, only.nc$file[1], sep = ""))
# what's in this data anyway? 

print(data)

## so what's in this first file? 

# this file have 18 variables, excluding dimensional variables
# 1. cruise name, 2. grid mapping name, 3. code for organisation, 4.profile label, 5.seadatanet ID, 
# 6. external resource link, 7. bathymetric depth @ profile site, 8. latitude, 9. longitude,  10.seadatanet quality flag, 
# 11. sea-water pressure due to sea water, 12. chronological julian date, 13.seadatanet quality flag, 14. SeaDataNet quality flag 
# 15. SeaDataNet quality flag ,16. P_sal (practical salinity), 17. SeaDataNet quality flag, 18. WC_temp_CTD

# OF THIS I PROBABLY NEED:
# 7 (depth), 8 & 9 (location), 12 (date) 16 (salinity), 18 (temperature) == 5 of the 18 variables. 



####
#6 dimensions,but they have no dimvar (dimension variables?!)

# There are global attributes, none of which seem useful. 

# SO WHERE IS THE ACTUAL DATA? 

install.packages("chron")
library(chron)
# USING THE variable-get function, let's have a look at the varibles we want
 
ctd.date <- ncvar_get(data, "TIME")
data$var$TIME$units # get the origin date
ctd.date.normal <- month.day.year(jul = ctd.date, origin = c(month = 1, day = 1, year = -4713)) # convert to something useful
ctd.date.vector <- paste(ctd.date.normal$year, ctd.date.normal$month, ctd.date.normal$day, sep = "-")

depth <- ncvar_get(data, "SDN_BOT_DEPTH")
location.lat <- ncvar_get(data, "LATITUDE")
location.lon <- ncvar_get(data, "LONGITUDE")
salinity <- ncvar_get(data, "PSALPR01")
temp <- ncvar_get(data, "TEMPST01")



# IDEALLY, WHAT WOULD I CREATE? 

# I want to have a data frame with: 
# date, lat, long, depth, conductivity, temperature
# I can then plot: total coverage (all data points ever), year-ID'd data
# number of entries per year @ different depths

collated <- data.frame(FILEREF = c(1:length(only.nc$file)), DATE_julian = NA, Date_normal = NA, 
           LAT = NA, LONG = NA, CON = NA, DEPTH = NA, TEMP = NA)


collated[1,] <- c(only.nc$file[1], ctd.date, ctd.date.vector, location.lat, location.lon, length(salinity), depth, length(temp))


# what if i try this with the next date set? is it the same?

for (i in 1:length(only.nc$file)) {
  #library(chron)
  print(i)
  temp <- nc_open(file = paste(path.to.data, only.nc$file[i], sep = ""))
  
  # stupid date 
  ctd.date <- ncvar_get(temp, "TIME")
  temp$var$TIME$units # get the origin date
  time.split <- strsplit(temp$var$TIME$units, split = "T")[[1]][1]
  time.split <- strsplit(time.split, split = " ")[[1]][3]
  time.split <- strsplit(time.split, "-")
  time.split <- as.numeric(time.split[[1]])
  ctd.date.normal <- month.day.year(jul = ctd.date, origin = c(month = time.split[3], 
                                    day = time.split[4], year = -time.split[2]))
  ctd.date.vector <- paste(ctd.date.normal$year, ctd.date.normal$month, ctd.date.normal$day, sep = "-")
  
  # rest of the actual data
  depth <- NA
  location.lat <- NA
  location.lon <- NA
  salinity <- NA
  temperature <- NA
  
  
  
  depth <- try(ncvar_get(temp, "SDN_BOT_DEPTH"))
  location.lat <- try(ncvar_get(temp, "LATITUDE"))
  location.lon <- try(ncvar_get(temp, "LONGITUDE"))
  salinity <- try(ncvar_get(temp, "PSALPR01"))
  temperature <- try(ncvar_get(temp, "TEMPST01"))
  
  
  collated[i,] <- c(only.nc$file[1], ctd.date, ctd.date.vector, location.lat, 
                    location.lon, length(salinity), depth, length(temperature))
  
  
}

for (i in 1:length(collated$FILEREF)) {
  collated$YEAR[i] <- strsplit(collated$Date_normal[i], split = "-")[[1]][1] }

plot(collated$LAT, collated$LONG, col = collated$YEAR)

# get data ready


collated$LAT <- as.numeric(collated$LAT)
collated$LONG <- as.numeric(collated$LONG)
collated$YEAR <- as.numeric(collated$YEAR)

library(ggplot2)
library(maps)
library(sf)

install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("rgeos"
                 )
library(rnaturalearthdata)
library(rnaturalearth)
library(rgeos)

### COPIED FROM TUTORIAL
# http://sarahleejane.github.io/learning/r/2014/09/20/plotting-beautiful-clear-maps-with-r.html
# adapted with: 
# https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'deepskyblue', colour = 'blue'), 
        axis.line = element_line(colour = "blue"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())


world2 <- ggplot(data = world) +
  geom_sf() + 
  geom_point(data = collated, aes(x = LONG, y = LAT), size = 2, shape = 23, fill = "white") +
  coord_sf(xlim = c(-70,-30), ylim = c(-70, -50), expand = F) + 
  cleanup

world2


#Longitude: 	  -63 (min) to -36 (max)
#Latitude:	-54 (max) to -70 (min)


# zoomed out version

map_data <-   base_world +
               geom_point(data=collated, 
               aes(x=LONG, y=LAT, colour = YEAR), 
               pch=23, size=5, alpha=I(0.7)) + 
              coord_sf(xlim = c(-70,-30), ylim = c(-70, -50), expand = F)  
  

map_data


ggsave("../Figures/CTD_locations.png", plot = map_data)
ggsave("../Figures/CTD_locations_otherversion.png", plot = world2)

# ok we're getting there but we need to zoom in a bit as this is absurd



unique(collated$YEAR)

map <- sf

length(unique(collated$Date_normal))


mappins <- raster(t(collated), xmn=min(LONG), xmx=max(LONG), ymn=min(LAT), 
             ymx=max(LAT), 
             crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))




