library(lubridate)
library(ggplot2)
library(dplyr)

#### data directory ----
# parent directory

dirData <- "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/kirkland_ecohydro"

# sap flow
sapRaw <- read.csv(paste0(dirData,"/sapflow/09_08_2022/Sapflow_TableDT.dat"),
                  header=FALSE,skip=4,na.strings=c("NAN"))

# weather station
weather <- read.csv(paste0(dirData, "/weather/z6-10463(z6-10463)-1694459136/z6-10463(z6-10463)-Configuration 1-1694459136.3651896.csv"), 
                    skip=3, header=FALSE)

weatherNames <- read.csv(paste0(dirData, "/weather/z6-10463(z6-10463)-1694459136/z6-10463(z6-10463)-Configuration 1-1694459136.3651896.csv"), 
                         nrows=3, header=FALSE)

basswoodmeas <- read.csv(paste0(dirData,"/allometry/basswoodmeas.csv"))
hemlockmeas <- read.csv(paste0(dirData,"/allometry/hemlockmeas.csv"))

#### organize sap flow ----

# remove empty sensor columns
datSap <- sapRaw[,1:18]


#parse date
datSap$dateF <- ymd_hms(datSap$V1)
datSap$year <- year(datSap$dateF)
datSap$doy <- yday(datSap$dateF)
datSap$hour <- hour(datSap$dateF)+(minute(datSap$dateF)/60)
datSap$DD <- datSap$doy + (datSap$hour/24)

colnames(datSap ) <- c("date","record",paste0("dT",seq(1,16)), "dateF", "year", "doy", "hour", "DD")
# subset for when all sensors were collecting data
datSap <- datSap %>%
  filter(doy >= 160 & year == 2022)
