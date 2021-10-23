
######################
### set the environment


graphics.off()
rm(list = ls())
load("pathway.Rdata") # data created in CTD*exploratory

setwd(pathway.to.linux)

# now set actually set the working directory within linux operating system
setwd("PhDCH1/Code/")

######################
# and load in my DF of all the relevant CTD files


FAO <- read.csv2(file = "../Data/FAO_AntarcticaFisheriesData.csv", sep = ",")

unique(FAO$ASFIS.species..Name.)

FFao <- read.csv2(file = "../Data/FAO_FISH.csv", sep = ",", stringsAsFactors = F)


unique(FFao$ASFIS.species..Name.) #190 species

# According to the FAO ' Global production by production source' database, 
# 57 countries have extracted marine resources from around the antarctic
# with 190 different species being targeted since 1950


unique(FFao$Country..Name.)

rel.fish <- c("Antarctic krill")

ffao.krill <- subset(FFao, FFao$ASFIS.species..Name. == "Antarctic krill")


2019-1950+1 # 70 years of data

global.krill <- data.frame(Year = 1950:2019, catch = NA)

for (i in 1:70) {
  j <- (i * 2) + 4
  temp <- as.numeric(ffao.krill[,j])
  temp <- na.omit(temp)
  global.krill$catch[i] <-   sum(temp)
}

plot.new()
plot(x = global.krill$Year, y = global.krill$catch/1000)

#######################

#icefish

ice.fao <- read.csv2("../Data/IcefishOnly.csv", sep = ",")


global.ice <- data.frame(Year = 1950:2019, catch = NA)

for (i in 1:70) {
  j <- (i * 2) + 4
  temp <- as.numeric(ice.fao[,j])
  temp <- na.omit(temp)
  global.ice$catch[i] <-   sum(temp)
}

plot.new()
plot(x = global.ice$Year, y = global.ice$catch/1000)

unique(ice.fao$Unit..Name.)



global.other <- 

plot.new()
par(mfrow = c(2,1))

         