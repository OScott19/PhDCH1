## following instruction :  

#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
#                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))


### usual setup

graphics.off()
rm(list = ls())
load("pathway.Rdata") # data created in CTD*exploratory

setwd(pathway.to.linux)

# now set actually set the working directory within linux operating system
setwd("PhDCH1/Code/")
path.to.data <- "../Data/BODC/AllCDTData/"


## load in most recent master df

load("../Data/CTD_master_211024_post1991.Rdata")

## library the relevant packages

library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")
#install.packages("rgeos")
library(rgeos)
library(viridisLite) # this has colour palattes

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

sites <- data.frame(lat = master.df$LAT, long = master.df$LONG)

## let's create a table of lat/long combinations

latlong <- paste(master.df$LAT, master.df$LONG, setp = "")
ll.tab <- as.data.frame(table(latlong))

unique.ll <- strsplit(as.character(ll.tab$latlong), split = " ")
check <- do.call(rbind, unique.ll) #yes!
check <- as.data.frame(check)
ll.tab$lat <- as.numeric(check$V1)
ll.tab$long <- as.numeric(check$V2)


### now let's plot them all! 


ggplot(data = world) +
  geom_sf() +
  geom_point(data = ll.tab, aes(x = long, y = lat, colour = Freq), size = 2, 
             shape = 18) +
  scale_colour_gradientn(colours = turbo(n= 100)) +
  coord_sf(xlim = c(-20, -79), ylim = c(-45, -70), expand = FALSE)


ggsave(file = "../Figures/HeatMapCTDlocations.png")



########################### also want to see how many discrete times each location was sampled


## let's create a table of lat/long combinations

latlongtime <- paste(master.df$LAT, master.df$LONG, master.df$DATE)
llt.tab <- as.data.frame(table(latlong)) # same number as the ll.tab, so map will be identical 

