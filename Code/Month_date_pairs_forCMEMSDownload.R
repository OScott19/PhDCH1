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

load("../Data/CTD_master_211108_v3_AllConverted.Rdata")



###isolate month/year pairings

m.y <- paste(master.df$MONTH, master.df$YEAR, sep = "-")
m.y <- unique(m.y)

### 93 unique months

### need dates in this format: "1993-01-01 12:00:00"

## how many days are there in each month? 
# 28: 02 
# 29: 02 (if leap year)
# 30: 04, 06, 09
# 31: 01, 03, 05, 07, 08, 10, 11, 12

##### how do add the first & last dates

#install.packages("hash")
library(hash)


months <- hash()

.set(months, keys = 1:12, values=c(31,28,31, 30,31,30,31,31,30,31,31,31))

i <- 2
i <- as.character(i)
months[[i]]

### ok now let's create that data frame

mys <- strsplit(m.y, split = "-")

mys.m <- unlist(mys)[ c(TRUE,FALSE) ]
mys.y <- unlist(mys)[ c(FALSE, TRUE) ]


dates <- data.frame(MONTH = mys.m, 
                    YEAR = mys.y, 
                    FIRST = 01, LAST = NA, START= NA, FINISH = NA) 

for (i in 1:12) {
  j <- as.character(i)
  dates$LAST[dates$MONTH == i] <- months[[j]] 
}

# now need to account for leap years

for (i in 1:length(dates$MONTH)) {
  if (dates$MONTH[i] == 2 && as.numeric(dates$YEAR[i]) %% 4 == 2) {
    dates$LAST[i] <- 29
    print("Found me a leap year")
  }
    
}


# now we assemble! Reminder: need dates in this format: "1993-01-01 12:00:00"


dates$START <- paste(dates$YEAR, "-", dates$MONTH, "-", dates$FIRST, " 12:00:00", sep = "")
dates$FINISH <- paste(dates$YEAR, "-", dates$MONTH, "-", dates$LAST, " 12:00:00", sep = "")
dates$FILENAME <- paste(dates$YEAR, dates$MONTH, "CMEMS", sep ="_")


## save this down 

## just load in relevant files (for bash script)

write.csv2(dates, file = "../Data/datesforCMEMS.csv")

# can I just create a monster file with all the bash scripts in it.. then I dont need to read anything
scripts <- data.frame(SCRIPTS = c(1:93))

for (i in 1:length(dates$MONTH)) {
  scripts$SCRIPTS[i] <- paste("python3 -m motuclient --motu https://my.cmems-du.eu/motu-web/Motu --service-id GLOBAL_REANALYSIS_PHY_001_030-TDS --product-id global-reanalysis-phy-001-030-daily --longitude-min -70 --longitude-max -21 --latitude-min -67 --latitude-max -50 --date-min ", dates$START[i],  "--date-max ", dates$FINISH[i], " --depth-min 0.494 --depth-max 5727.917 --variable bottomT --variable mlotst --variable so --variable thetao --variable uo --variable vo --out-dir ", dates$FILENAME[i], " --out-name CMEM_030_WholeModel --user oscott --pwd CMEMS_SCOTT_2021", sep = "")
  days$SCRIPTS[i] <- paste('python3 -m motuclient --motu https://my.cmems-du.eu/motu-web/Motu --service-id GLOBAL_REANALYSIS_PHY_001_030-TDS --product-id global-reanalysis-phy-001-030-daily --longitude-min -70 --longitude-max -21 --latitude-min -67 --latitude-max -50 --date-min "', daydates[i], ' 00:00:01" --date-max "', daydates[i], ' 12:00:00" --depth-min 0.494 --depth-max 5727.917 --variable bottomT --variable mlotst --variable so --variable thetao --variable uo --variable vo --out-dir CMEM_030_Nov --out-name ', daydates[i], '_CMEMS_030 --user oscott --pwd CMEMS_SCOTT_2021', sep = "")
  
  }

write.csv2(scripts, file = "../Data/CallScripts.csv")

scripts$SCRIPTS[1]



#### A SINGLE MONTH IS STILL TOO BIG - NEED TO NARROW IT DOWN TO (APPRENTLY) HALF A DAY?!

## let's also narrow the lats & longs to match my data

max(master.df$LAT)
min(master.df$LAT)
min(master.df$LONG)
max(master.df$LONG)

#so, -50 to -67 lat
# -21 to -70 long

# version 1: 570935.0Mb
# version 2:  423343.0Mb
## TOMORROW - get all of the unique dates that I have data for and 

daydates <- unique(master.df$DATE)

days <- data.frame(DAYS = c(1:length(daydates)))

for (i in 1:length(daydates)) {
  days$SCRIPTS[i] <- paste('python3 -m motuclient --motu https://my.cmems-du.eu/motu-web/Motu --service-id GLOBAL_REANALYSIS_PHY_001_030-TDS --product-id global-reanalysis-phy-001-030-daily --longitude-min -70 --longitude-max -21 --latitude-min -67 --latitude-max -50 --date-min "', daydates[i], ' 00:00:01" --date-max "', daydates[i], ' 12:00:00" --depth-min 0.494 --depth-max 5727.917 --variable bottomT --variable mlotst --variable so --variable thetao --variable uo --variable vo --out-dir CMEM_030_Nov --out-name ', daydates[i], '_CMEMS_030 --user oscott --pwd CMEMS_SCOTT_2021', sep = "")
}

days$SCRIPTS[1]


#website new script (fewer variables, one day only, new range)

# ONE DAY

python3 -m motuclient --motu https://my.cmems-du.eu/motu-web/Motu --service-id GLOBAL_REANALYSIS_PHY_001_030-TDS --product-id global-reanalysis-phy-001-030-daily --longitude-min -70 --longitude-max -21 --latitude-min -67 --latitude-max -50 --date-min "1995-02-14 12:00:00" --date-max "1995-02-15 12:00:00" --depth-min 0.494 --depth-max 5727.917 --variable bottomT --variable so --variable thetao --out-dir CMEM_030_Nov --out-name Test_Model_Nov21 --user oscott --pwd CMEMS_SCOTT_2021


# ONE MONTH
python3 -m motuclient --motu https://my.cmems-du.eu/motu-web/Motu --service-id GLOBAL_REANALYSIS_PHY_001_030-TDS --product-id global-reanalysis-phy-001-030-daily --longitude-min -70 --longitude-max -21 --latitude-min -67 --latitude-max -50 --date-min "1995-02-14 12:00:00" --date-max "1995-02-28 12:00:00" --depth-min 0.494 --depth-max 5727.917 --variable bottomT --variable so --variable thetao --out-dir CMEM_030_Nov --out-name Test_Model_Nov21 --user oscott --pwd CMEMS_SCOTT_2021

print(daydates[1:12])



# ok now to try fixign the apostrophes

for (i in 1:length(dates$MONTH)) {
  scripts$BYMONTH[i] <- paste('python3 -m motuclient --motu https://my.cmems-du.eu/motu-web/Motu --service-id GLOBAL_REANALYSIS_PHY_001_030-TDS --product-id global-reanalysis-phy-001-030-daily --longitude-min -70 --longitude-max -21 --latitude-min -67 --latitude-max -50 --date-min "', dates$START[i], '" --date-max "', dates$FINISH[i], '" --depth-min 0.494 --depth-max 5727.917 --variable bottomT --variable mlotst --variable so --variable thetao --variable uo --variable vo --out-dir CMEM_030_Nov --out-name ', dates$FILENAME[i], '_CMEMS_030 --user oscott --pwd CMEMS_SCOTT_2021', sep = "")
  }

print(scripts$BYMONTH[1]) # doesn't do this right
cat(scripts$BYMONTH[1]) # use cat to corrctly print the quotations - paste does 

#### OK THIS IS SO MUCH BETTER. And i might even be able to get all of these downloaded properly 

scripts2 <- scripts$BYMONTH
write.csv2(scripts2, file = "../Data/Scripts2.sh")


#### working with this script now: 
for line in `cat head -5 Scripts2.csv`; do echo $line; done

IN COMMAND LINE
# this should be do-able 


temp <- read.csv2("../Data/Scripts2.csv")
a <- temp$x[93]
b <- temp$x[1]
## ok script all together, should be do-able

python3 -m motuclient --motu https://my.cmems-du.eu/motu-web/Motu --service-id GLOBAL_REANALYSIS_PHY_001_030-TDS --product-id global-reanalysis-phy-001-030-daily --longitude-min -70 --longitude-max -21 --latitude-min -67 --latitude-max -50 --date-min "', dates$START[i], '" --date-max "', dates$FINISH[i], '" --depth-min 0.494 --depth-max 5727.917 --variable bottomT --variable mlotst --variable so --variable thetao --variable uo --variable vo --out-dir CMEM_030_Nov --out-name ', dates$FILENAME[i], '_CMEMS_030 --user oscott --pwd CMEMS_SCOTT_2021', sep = ""

python3 -m motuclient --motu https://my.cmems-du.eu/motu-web/Motu --service-id GLOBAL_REANALYSIS_PHY_001_030-TDS --product-id global-reanalysis-phy-001-030-daily --longitude-min -70 --longitude-max -21 --latitude-min -67 --latitude-max -50 --date-min "1995-2-1 12:00:00" --date-max "2017-12-31 12:00:00" --depth-min 0.494 --depth-max 5727.917 --variable bottomT --variable mlotst --variable so --variable thetao --variable uo --variable vo --out-dir CMEM_030_Nov --out-name 1995_2_CMEMS_CMEMS_030 --user oscott --pwd CMEMS_SCOTT_2021

