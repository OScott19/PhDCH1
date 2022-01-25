
###### RIGHT - LETS MOVE ON TO THE QUALITATIVE ANALYSIS: LOOKING AT THIINGS

## let's plot all of our locations that we are currently looking at 

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(chron)
library(data.table)
library(gridExtra)

## and for plotting

library(grid)
library(ggpubr)
library(patchwork)
library("Metrics")

library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")
#install.packages("rgeos")
library(rgeos)
library(viridisLite) # this has colour palattes
library(RColorBrewer)
library(viridis)


load("pathway.Rdata") # data created in CTD*exploratory

setwd(pathway.to.linux)

# now set actually set the working directory within linux operating system
setwd("PhDCH1/Code/")
path.to.data <- "../Data/BODC/AllCDTData/"


### load in my data set
load("../Data/RefinedOverlapData_2022.Rdata")




world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

sites <- data.frame(lat = r.overlap$ctdLAT, long = r.overlap$ctdLON)

### now let's plot them all! 


plot <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = long, y = lat), size = 2, 
             shape = 18) +
  #scale_colour_gradientn(colours = turbo(n= 1)) +
  coord_sf(xlim = c(-20, -79), ylim = c(-45, -70), expand = FALSE)


drake <- data.frame(Location = "Drake Passage", x = c(-69.5, -69.5, -67.3, -67.3), y = c(-61, -56, -56, -61))
georgia <- data.frame(Location = "South Georgia", x = c(-33, -33, -40,-40), y = c(-55.1, -52.5, -52.5, -55.1))
orkney <- data.frame(Location = "South Orkneys", x = c(-47.1, -47.1, -41.5, -41.5), y = c(-61.5, -59.5, -59.5, -61.5))
mid.sea.1 <- data.frame(Location = "Mid Sea East", x = c(-44, -44, -40, -40), y = c(-59, -56, -56, -59))
sandwitch.arc <- data.frame(Location = "South Sandwitch Arc", x = c(-30, -30, -35, -35), y = c(-55.1, -59.5, -59.5, -55.1))
mid.sea.diagonal  <- data.frame(Location = "Mid Sea West", x = c(-59.5, -56, -52.5, -56), y = c(-54.5, -62.5, -62.5, -54.5)) 

location.df <- rbind(drake, georgia, orkney, mid.sea.1, sandwitch.arc, mid.sea.diagonal)

# labels
polygon.labs <- data.frame(NAME = c(1:6), 
                           LON = c(min(drake$x), min(georgia$x), min(orkney$x), min(mid.sea.1$x), min(sandwitch.arc$x), -56),
                           LAT = c(min(drake$y), min(georgia$y), min(orkney$y), min(mid.sea.1$y), min(sandwitch.arc$y), min(mid.sea.diagonal$y))
                           )
plot2 <- ggplot(data = world) +
   geom_sf() +
   geom_point(data = sites, aes(x = long, y = lat), size = 2, shape = 18) +
   coord_sf(xlim = c(-20, -79), ylim = c(-45, -70), expand = FALSE) + 
   geom_polygon(data = location.df, aes(x, y, group = Location, fill = Location), alpha = 0.5)    +
   #geom_label(data = polygon.labs, aes(LON, LAT, label = NAME), size = 5, fontface = "bold") +
   labs(x = "Longitude", y = "Latitude", colour = "Location") 


plot2

ggsave(file = "../Figures/CTD_Model_verificationSites_220124.png")


################## now going to plot the thetaS plots for the selected highlighted zone




 ## set up #: 
 # let's try an automatic palate generator 
 
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
 
 d.mod.plot <- qplot(y = drake.df$TEMP.1, x = drake.df$SAL.m1)
 d.data.plot <- qplot(y = drake.df$ctdTEMP, x = drake.df$ctdSAL)
 
 ## now let's do it a bit more nicely 
 
 drake.df$YEARS <- as.numeric(substring(drake.df$DATE, 1, 4))
 d.years <- sort(unique(drake.df$YEARS))
 
 # work this bit out after the quick plot
 d.range <- data.frame(y = c(0, 10), x = c(33, 35))
 
 #d.mod.plot2 <- 
    ggplot(data = drake.df, aes(y =TEMP.m1, x = SAL.m1))  + 
      geom_point(aes(colour = as.factor(YEARS))) +
      labs(x = "Practical Salinity", y = "Temperature") + 
      scale_y_continuous(name = "Temperature", limits = c(min(d.range$y), max(d.range$y))) + 
      scale_x_continuous(limits = c(min(d.range$x), max(d.range$x)))    +               
      geom_label(data = d.range, aes(x = mean(x), y = max(y), label = "Model Data"), size = 5, fontface = "bold") + 
      scale_color_manual(name = "Years", values = col_vector[1:length(d.years)])
    
 
 d.data.plot2 <- 
    ggplot(data = drake.df, aes(y = ctdTEMP, x = ctdSAL))  + 
   geom_point(aes(colour = as.factor(YEARS))) +
   labs(y = "Temperature", x = "Salinity") + 
    scale_y_continuous(name = "Temperature", limits = c(min(d.range$y), max(d.range$y))) + 
    scale_x_continuous(limits = c(min(d.range$x), max(d.range$x)))  +  
    geom_label(data = d.range, aes(x = mean(x), y = max(y), label = "CTD Data"), size = 5, fontface = "bold") + 
      scale_color_manual(name = "Years", values = col_vector[1:length(d.years)])
 
 
 ## can also do this with Oce aparently 
 
 library(oce)
 library(seacarb)
 
 d.mod.simple <- data.frame(temp = drake.df$TEMP.m1, sal = drake.df$SAL.m1)
 ggplot2::ts_pl
 plotTS(x = as.ctd(d.mod.simple))
 pressures <- seacarb::d2p(drake.df$modDEPTH, lat = drake.df$modLAT.m1)
 forTS <- as.ctd(sal = d.mod.simple$sal, temperature = d.mod.simple$temp, 
        pressure = seacarb::d2p(drake.df$modDEPTH, lat = drake.df$modLAT.m1), time =  drake.df$YEARS) 
    
 marOrig <- par("mar") 
# cm <- colormap(section[['oxygen']])
 plotTS(forTS,col = forTS@data$time, mar=par('mar'))  # so this is a bit more legit 
 
 plotTS()
 
 
 #library(ggpubr)
 drake.thetaS <- ggarrange(d.data.plot2, d.mod.plot2, common.legend = T, legend = "right")
 ggsave(drake.thetaS, filename = "../Figures/drake_thetaoSalinity_yearCol_20220124.png")
 
 
 
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
 
 
 l
 ggsave(combined2, filename = "../Figures/mid.sea_thetaoSalinity_yearCol.png")