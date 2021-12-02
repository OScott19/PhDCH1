#### set up

graphics.off()
rm(list = ls())

## load in some packages

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(chron)
library(data.table)
library(gridExtra)


load("pathway.Rdata") # data created in CTD*exploratory

setwd(pathway.to.linux)

# now set actually set the working directory within linux operating system
setwd("PhDCH1/Code/")
path.to.data <- "../Data/BODC/AllCDTData/"


###### now, let's load in our matched dataset

load("../Data/CMEMS_All4_File_92_of92.Rdata")

length(unique(all.model.data$UNIQ)) * 50
all.model.data <- all.model.data[-c(1,2),]
length(all.model.data$DATE[is.na(all.model.data$DATE)]) # 91 - one per loop? 
(length(all.model.data$UNIQ)-91)/50 # 756 

# so this feels like there are only 756 unique dates 

load("../Data/CTD_master_211108_v3_AllConverted.Rdata")

# how many unique dates are there in the overlapping ctd data?

ctd.overlap <- master.df[master.df$YEAR >= 1993,]

length(unique(ctd.overlap$DATE)) # 746!!! so actually this is all good. MARVELLOUS

## perhaps I didn't download as wide of a geographic area as I thougt I did
#this may explain the missing unique IDs from earlier

# let's check max/min lats & longs

model <- nc_open(file = "D:/CMEMS_001_030_MONTHLY/CMEMS-GLOBAL_001_030-bottomT_thetao_so-200001.nc")
lats <- ncvar_get(model, "latitude")
min(lats) #-67
max (lats)#-50
longs <- ncvar_get(model, "longitude")
min(longs)#-70
max(longs)#-21

max(ctd.overlap$LAT) #-50
min(ctd.overlap$LAT) # -66.8
min(ctd.overlap$LONG) # -69.9
max(ctd.overlap$LONG) #-21.23

# ALL SEEMS TO BE MATCHING & IN ORDER. GREAT. 

# LET'S MOVE ON TO SENSE-CHECK THE MATCHING PROCESS

all.model.data$diffA <- abs(all.model.data$modDEPTH - all.model.data$ctdDEPTH) # want absolute for calcs
all.model.data$diffP <- all.model.data$diffA/ all.model.data$modDEPTH 
max(all.model.data$diffP, na.rm = T) # 100 - that's pretty big 

library(ggplot2)

# let's look at matching gaps accross the depths

qplot(x = all.model.data$modDEPTH, y = all.model.data$diffA)
qplot(x = all.model.data$modDEPTH, y = all.model.data$ctdDEPTH)
all.model.data$COL <- "black"
all.model.data$COL[all.model.data$diffP > 0.5] <- "blue"
all.model.data$COL[which (all.model.data$modDEPTH > 2000 | all.model.data$diffP > 0.25) ] <- 3
all.model.data$COL[which (all.model.data$modDEPTH > 4000 | all.model.data$diffP > 0.1) ] <- 4

accuracy <- as.data.frame(table(all.model.data$COL))
all.model.data$COL <- as.factor(all.model.data$COL)

boringCols <- list(c("black", "grey"))
ggplot( data = all.model.data) + 
  geom_point(aes(x = modDEPTH, y = ctdDEPTH, color = COL))
             ) + 
  scale_colour_discrete(boringCols)



qplot(x = all.model.data$modDEPTH, 
      y = all.model.data$ctdDEPTH, 
      colour = all.model.data$COL)

hist(all.model.data$diffP)


## so clearly some of the matches aren't great


## let's slim it down, so ctd can only be matched if the difference is <50% of the value of the matched depth
# r = refined
r.overlap <- all.model.data[all.model.data$diffP < 0.5,] # from 37k to 13k - lost c.60%

#let's replot these

plot(x = r.overlap$modDEPTH, y = r.overlap$diffA)
plot(x = r.overlap$modDEPTH, y = r.overlap$diffP)
qplot(x = r.overlap$modDEPTH, y = r.overlap$ctdDEPTH)

# gray/black plot showing what was removed during cleaning 

## SO MUCH TIGHTER!! 

save(r.overlap, file = "../Data/RefinedOverlapData.Rdata")

hist(r.overlap$ctdDEPTH)
hist(r.overlap$modDEPTH)

# oh good,they look similar!!



load("../Data/RefinedOverlapData.Rdata")




# NEXT STEPS 
# do some R squared stuff to compare accross: time, depths & whole data frame
# let's also plot salinity vs temperature for all of this matched data
# ALSO LETS MAKE A MAP OF THESE LOCATIONS! 


### R tests 
install.packages("Metrics" )
library("Metrics")

rmse <- function(a,b) {
  sqrt(mean((a - b)**2))
}

#rms.package <- rmse(data$actual, data$predicted)
r.olhd <- r.overlap
r.overlap <- na.omit(r.overlap)


rms.sal <- rmse(r.overlap$ctdSAL,
                    r.overlap$SAL)

rms.temp <- rmse(r.overlap$ctdTEMP,
                r.overlap$TEMP)

rms.depth.sal <- c()
rms.depth.temp <- c()
depths <- sort(unique(r.overlap$modDEPTH))

for (d in 1:length(depths)) {
  temp  <- subset (r.overlap, r.overlap$modDEPTH == depths[d])
  rms.depth.sal<- c(rms.depth.sal, rmse(temp$SAL, temp$ctdSAL))
  rms.depth.temp <-c(rms.depth.temp, rmse(temp$TEMP, temp$ctdTEMP))
  }

rms_depths <- grid.arrange(qplot(x = depths, y = rms.depth.sal), qplot(x = depths, y = rms.depth.temp)
, nrow = 2)

ggsave(rms_depths, filename = "../Figures/RMS_acrossDepths.png")

qplot(x = depths, y = rms.depth.sal)
qplot(x = depths, y = rms.depth.temp)

# repeat the code above for years

years <- sort(as.numeric(unique(substring(r.overlap$DATE, 1, 4))))
r.overlap$YEARS <- as.numeric(substring(r.overlap$DATE, 1, 4))
rms.yearly.sal <- c()
rms.yeary.temp <- c()

for (y in 1:length(years)) {
  temp  <- subset (r.overlap, r.overlap$YEARS == years[y])
  rms.yearly.sal<- c(rms.yearly.sal, rmse(temp$SAL, temp$ctdSAL))
  rms.yeary.temp <-c(rms.yeary.temp, rmse(temp$TEMP, temp$ctdTEMP))
}

rms_years <- grid.arrange(qplot(x = years, y = rms.yearly.sal), 
                          qplot(x = years, y = rms.yeary.temp),
                          nrow = 2)

ggsave(rms_years, filename = "../Figures/RMS_acrossYears.png")





## no pattern over time 
# but definiteiy a pattern over depths


###### RIGHT - LETS MOVE ON TO THE QUALITATIVE ANALYSIS: LOOKING AT THIINGS

## let's plot all of our locations that we are currently looking at 

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

sites <- data.frame(lat = r.overlap$ctdLAT, long = r.overlap$ctdLON)

### now let's plot them all! 


plot <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = long, y = lat, colour = 1), size = 2, 
             shape = 18) +
  scale_colour_gradientn(colours = turbo(n= 1)) +
  coord_sf(xlim = c(-20, -79), ylim = c(-45, -70), expand = FALSE)

drake <- data.frame(x = c(-69.5, -69.5, -67.3, -67.3), y = c(-58, -56.7, -56.7, -58))
georgia <- data.frame(x = c(-37, -37, -39.7,-39.7), y = c(-54.2, -53, -53, -54.2))
orkney <- data.frame(x = c(-47.1, -47.1, -43.5, -43.5), y = c(-61.5, -59.5, -59.5, -61.5))
mid.sea <- data.frame(x = c(-44, -44, -40, -40), y = c(-59, -56, -56, -59))
  
plot +      
  geom_polygon(data = drake, aes(x, y, group = 1), fill = "red", color = "white", alpha = 0.5)    +
  geom_polygon(data = georgia, aes(x, y, group = 1), fill = "blue", color = "white", alpha = 0.3)   +
  geom_polygon(data = orkney, aes(x, y, group = 1), fill = "green", color = "white", alpha = 0.3) + 
  geom_polygon(data = mid.sea, aes(x, y, group = 1), fill = "yellow", color = "white", alpha = 0.3)
  
                    
#ggsave(file = "../Figures/CTD_Model_verificationSites.png")


# LET'S PICK THREE (NOW FIVE - SEE BELOW) AREAS on this map to look at the temp/salinity gradient
# and also to plot what is going on with temperature across the depths accross a line


## three sites: 
#-middle of drake passage
# south gorgia
# south orkneys
# middle of scoia sea somwhere
# + A23 line - ONLY DO THIS FOR PLOTTING SAL/TEMP OVER THE LINE 

## set up #: 
# let's try an automatic palatte generator 

library(RColorBrewer)
n <- 25 #  unlikely to have more years than this 
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00") #, "#CC79A7")
words <- c("red", "blue", "grey", "green", "skyblue", "purple", "orange")


# so now I have my drake passage limits, now I find the model/ctd matches that correspond
#r.overlap$ctdLAT[123] # 4 decimal places
drake.lats <- seq(min(drake$y), max(drake$y), 0.00001)
drake.longs <- seq(min(drake$x), max(drake$x), 0.00001)

drake.df <- subset(r.overlap, r.overlap$ctdLAT %in% drake.lats)
drake.df <- subset(drake.df, drake.df$ctdLON %in% drake.longs)

# that's ok! 

## PLOTTING DRAKE DATA
# let's do the thetao-watsit plots for these two

## we want to plot temperature vs salinity - real & model!

d.mod.plot <- qplot(x = drake.df$TEMP, y = drake.df$SAL)
d.data.plot <- qplot(x = drake.df$ctdTEMP, y = drake.df$ctdSAL)

## now let's do it a bit more nicely 


drake.df$YEARS <- as.numeric(substring(drake.df$DATE, 1, 4))
d.years <- sort(unique(drake.df$YEARS))

d.mod.plot2 <- ggplot(data = drake.df, aes(x =TEMP, y = SAL))  + 
  geom_point(aes(colour = as.factor(YEARS))) +
  #scale_color_manual(name = "Years", values = words)
  scale_color_manual(name = "Years", values = col_vector[1:length(d.years)])


d.data.plot2 <- ggplot(data = drake.df, aes(x = ctdTEMP, y = ctdSAL))  + 
  geom_point(aes(colour = as.factor(YEARS))) +
  scale_color_manual(name = "Years", values = col_vector[1:length(d.years)])

drake.thetaS <- grid.arrange(d.mod.plot2, d.data.plot2, ncol = 2)
ggsave(drake.thetaS, filename = "../Figures/drake_thetaoSalinity_yearCol.png")



# NOW GEORGIA DATA
georgia.lats <- seq(min(georgia$y), max(georgia$y), 0.00001)
georgia.longs <- seq(min(georgia$x), max(georgia$x), 0.00001)

georgia.df <- subset(r.overlap, r.overlap$ctdLAT %in% georgia.lats)
georgia.df <- subset(georgia.df, georgia.df$ctdLON %in% georgia.longs)
# 78 points - better

g.mod.plot <- qplot(x = georgia.df$TEMP, y = georgia.df$SAL, col = georgia.df$d)
g.data.plot <- qplot(x = georgia.df$ctdTEMP, y = georgia.df$ctdSAL)



georgia.df$YEARS <- as.numeric(substring(georgia.df$DATE, 1, 4))
g.years <- sort(unique(georgia.df$YEARS))

g.mod.plot2 <- ggplot(data = georgia.df, aes(x =TEMP, y = SAL))  + 
                    geom_point(aes(colour = as.factor(YEARS))) +
                    scale_color_manual(name = "Years", values = col_vector[1:length(g.years)])

    
g.data.plot2 <- ggplot(data = georgia.df, aes(x = ctdTEMP, y = ctdSAL))  + 
                    geom_point(aes(colour = as.factor(YEARS))) +
                    scale_color_manual(name = "Years", values = col_vector[1:length(g.years)])

georgia.thetaS <- grid.arrange(g.mod.plot2, g.data.plot2, ncol = 2)
ggsave(georgia.thetaS, filename = "../Figures/georgia_thetaoSalinity_yearCol.png")


## plotting orkney data

orkney.lats <- seq(min(orkney$y), max(orkney$y), 0.00001)
orkney.longs <- seq(min(orkney$x), max(orkney$x), 0.00001)

orkney.df <- subset(r.overlap, r.overlap$ctdLAT %in% orkney.lats)
orkney.df <- subset(orkney.df, orkney.df$ctdLON %in% orkney.longs)
#460  points - better


# CAN COLOUR BY YEAR OR BY DEPTH - I HTINK YEAR MAY BE MORE INTERESTING 
o.mod.plot <- qplot(x = orkney.df$TEMP, y = orkney.df$SAL, col = orkney.df$YEARS)
o.data.plot <- qplot(x = orkney.df$ctdTEMP, y = orkney.df$ctdSAL, col = orkney.df$YEARS)

orkney.df$YEARS <- as.numeric(substring(orkney.df$DATE, 1, 4))
o.years <- sort(unique(orkney.df$YEARS))

o.mod.plot2 <- ggplot(data = orkney.df, aes(x =TEMP, y = SAL))  + 
  geom_point(aes(colour = as.factor(YEARS))) +
  scale_color_manual(name = "Years", values = col_vector[1:length(o.years)])


o.data.plot2 <- ggplot(data = orkney.df, aes(x = ctdTEMP, y = ctdSAL))  + 
  geom_point(aes(colour = as.factor(YEARS))) +
  scale_color_manual(name = "Years", values = col_vector[1:length(o.years)])

orkney.thetaS <- grid.arrange(o.mod.plot2, o.data.plot2, ncol = 2)
ggsave(orkney.thetaS, filename = "../Figures/orkney_thetaoSalinity_yearCol.png")


## PLOTTING MID SEA
## edit code below
## we want to plot temperature vs salinity - real & model!

mid.sea.lats <- seq(min(mid.sea$y), max(mid.sea$y), 0.00001)
mid.sea.longs <- seq(min(mid.sea$x), max(mid.sea$x), 0.00001)

mid.sea.df <- subset(r.overlap, r.overlap$ctdLAT %in% mid.sea.lats)
mid.sea.df <- subset(mid.sea.df, mid.sea.df$ctdLON %in% mid.sea.longs)
# 78 points - better

ms.mod.plot <- qplot(x = mid.sea.df$TEMP, y = mid.sea.df$SAL, col = mid.sea.df$d)
ms.data.plot <- qplot(x = mid.sea.df$ctdTEMP, y = mid.sea.df$ctdSAL)

mid.sea.df$YEARS <- as.numeric(substring(mid.sea.df$DATE, 1, 4))
ms.years <- sort(unique(mid.sea.df$YEARS))

ms.mod.plot2 <- ggplot(data = mid.sea.df, aes(y = TEMP, x = SAL))  + 
  geom_point(aes(colour = as.factor(YEARS))) +
  scale_color_manual(name = "Years", values = col_vector[1:length(ms.years)]) +
  labs(title = "Copernicus model", x = "Salinity (ppt)", y = "Temperature (°C)") +
  xlim(34, 35) + 
  ylim (-0.5, 2.5) + 
  theme(legend.position = "none") 


ms.data.plot2 <- ggplot(data = mid.sea.df, aes(y = ctdTEMP, x = ctdSAL))  + 
  geom_point(aes(colour = as.factor(YEARS))) +
  scale_color_manual(name = "Years", values = col_vector[1:length(ms.years)]) + 
  labs(title = "CTD data", x = x = "Salinity (ppt)", y = "") +
  xlim(34, 35) + 
  ylim (-0.5, 2.5) + 
  theme(legend.position = "none")


# want to have them share a legend


combined <- ms.mod.plot2 + ms.data.plot2 & theme(legend.position = "right")
combined2 <- combined + plot_layout(guides = "collect") +
                plot_annotation(title = "ThetaS plot of the mid-Scotia Sea \n ", 
                theme = theme(plot.title = element_text(size = 17)))  
  



#mid.sea.thetaS <- grid.arrange(ms.mod.plot2, ms.data.plot2, ncol = 2, 
#                     top = textGrob("ThetaS plot of the mid-Scotia Sea \n ", 
#                             gp=gpar(fontsize=17 ,font=3)), 
#                     left = "Temperature (°C)", 
#                     bottom = "Salinity (ppt)")


library(gridExtra)
library(grid)
library(ggplot2)
library(ggpubr)
library(patchwork)
install.packages("patchwork")

install.packages("ggpubr")
ggsave(combined2, filename = "../Figures/mid.sea_thetaoSalinity_yearCol.png")



