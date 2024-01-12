install.packages("remotes")
library(remotes)
remotes::install_github("the-Hull/TREX")

library(TREX)
library(dplyr)
library(lubridate)
library(ggplot2)
#### data directory ----
# parent directory
dirUser <- 2

dirData <- c("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/kirkland_ecohydro",
             "E:/Google Drive/research/projects/kirkland_ecohydro")

# sap flow
sapRaw <- read.csv(paste0(dirData[dirUser],"/sapflow/09_08_2022/Sapflow_TableDT.dat"),
                   header=FALSE,skip=4,na.strings=c("NAN"))

# weather station
weather <- read.csv(paste0(dirData[dirUser], "/weather/z6-10463(z6-10463)-1694459136/z6-10463(z6-10463)-Configuration 1-1694459136.3651896.csv"), 
                    skip=3, header=FALSE)



colnames(weather) <- c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                       "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                       "SensorTemp","VPD","BatPct","BatVolt","RefPr","LogTemp")



weatherInfo <- read.csv(paste0(dirData[dirUser], "/weather/z6-10463(z6-10463)-1694459136/z6-10463(z6-10463)-Configuration 1-1694459136.3651896.csv"), 
                        nrows=3, header=FALSE)
# allometry
basswoodmeas <- read.csv(paste0(dirData[dirUser],"/allometry/basswoodmeas.csv"))
hemlockmeas <- read.csv(paste0(dirData[dirUser],"/allometry/hemlockmeas.csv"))
basswoodlm <- read.csv(paste0(dirData[dirUser],"/allometry/DettmannMcfarlane.csv"))
# sensor info
sensors <- read.csv(paste0(dirData[dirUser],"/sapflow/sensordata_061522.csv"))



#### sapwood ----

#hemlock allometry from "Water use by eastern hemlock (Tsuga canadensis) 
#and black birch (Betula lenta): implications of effects of the hemlock woolly adelgid" by Daley et Al
#basswood allometry by Ewers et. Al: "Tree species effects on stand transpiration"

sapdepth <- lm(basswoodmeas$Sapwooddepth.cm ~ basswoodmeas$DBH.cm)

summary(sapdepth)

sapdepthHem <- lm(hemlockmeas$Sapwooddepth.cm ~ hemlockmeas$DBH.cm)

summary(sapdepthHem)

sensors$sd.cm <- ifelse(sensors$Tree.Type == "Hemlock", #if sensors is hemlock,
                        -0.0133 + (0.1252*sensors$DBH..cm.),
                        -0.7783 + (0.24546*sensors$DBH..cm.))
sensors$DBH.cm <- sensors$DBH..cm.
########## Tree allometry ----


## sapwood area

# hemlock tree
# sapwood area measurements are from Daley et Al 
plot(hemlockmeas$DBH.cm, log(hemlockmeas$SapwoodArea.cm2))
sapareaHem <- lm(log(hemlockmeas$SapwoodArea.cm2) ~ log(hemlockmeas$DBH.cm))
abline(sapareaHem)
summary(sapareaHem)# sapwood area calculations

sapcalc <- numeric()
bark <- numeric()
Htwd <- numeric()
# calculations result in cm2
for(i in 1:nrow(sensors)){
  if(sensors$Tree.Type[i] == "Basswood"){
    #calculate heartwood
    #Hengst and dawson 1993
    bark <- (sensors$DBH.cm[i]*0.0326) - 0.1708
    
    Htwd <- sensors$DBH.cm[i]  - (sensors$sd.cm[i]*2) - (bark*2)
    
    #calculate sapwood area
    
    sapcalc[i] <- (pi*(((sensors$sd.cm[i]/2)+(Htwd/2))^2))-(pi*((Htwd/2)^2))
    
  }else{
    # regression from Daly table
    sapcalc[i] <- exp(-1.192 + (2.010*log(sensors$DBH.cm[i])))
  }
  
}


#convert sap area from cm2 to m2
sensors$sap.aream2 <- 0.0001*sapcalc

### projected leaf area

# data from supplement in dettman mcfarlane
blm <- basswoodlm %>%
  filter(SPECIES =="Tilia americana")
plot(log(blm$DBH_CM), log(blm$LEAF_DRY_MASS_KG))

b.mod <- lm(log(blm$LEAF_DRY_MASS_KG)~log(blm$DBH_CM))
summary(b.mod)

biomass.kg <- numeric()
LA.m2 <- numeric()

# calculations result in m2
for(i in 1:nrow(sensors)){
  if(sensors$Tree.Type[i] == "Basswood"){
    
    biomass.kg = exp(-4.25 + 1.79*(log(sensors$DBH.cm[i])))
    
    
    #conversion from Ewers et al SLA averaged over two years (in m2/kg)
    LA.m2[i] <- ((34.8+32.4)/2)*biomass.kg
  }else{
    #leaf area in m2 for hemlock, from Ford and Vose 2007
    LA.m2[i] <- exp((1.542*log(sensors$DBH.cm[i]))-0.274)
  }
}
sensors$LA.m2 <- LA.m2

#### TREX ----


# remove empty sensor columns
datSap <- sapRaw[,1:18]


#parse date
datSap$date <- datSap$V1
datSap$dateF <- ymd_hms(datSap$date)
datSap$year <- year(datSap$dateF)
datSap$doy <- yday(datSap$dateF)
datSap$hour <- hour(datSap$dateF)+(minute(datSap$dateF)/60)
datSap$DD <- datSap$doy + (datSap$hour/24)

colnames(datSap ) <- c("date","record",paste0("dT",seq(1,16)), "dateF", "year", "doy", "hour", "DD")
# subset for when all sensors were collecting data
datSap <- datSap %>%
  filter(doy >= 160 & year == 2022)


# start with one sensor
datSens <- data.frame(timestamp=datSap$date,
                      value= datSap[,3])

raw <- is.trex(datSens, tz="UTC", time.format="%Y-%m-%d %H:%M:%S", solar.time=FALSE)
               
input <- dt.steps(input=raw, 
                  start="2022-06-18 00:00:00",
                  end="2022-09-06 00:00:00",
                  time.int=15,
                  max.gap=60,
                  decimals=10,
                  df=FALSE)

input[which(input<4 | input>13)]<- NA

input <- tdm_dt.max(input,
                    methods = c("pd", "mw", "dr"),
                    det.pd = TRUE,
                    interpolate = FALSE,
                    max.days = 10,
                    df = FALSE)

plot(input$input, ylab = expression(Delta*italic("V")))

lines(input$max.pd, col = "green")
lines(input$max.mw, col = "blue")
lines(input$max.dr, col = "orange")

plot(input$k.pd, ylim=c(0,1))
lines(input$k.mw, col="blue")
lines(input$k.dr, col="orange")


output.data<- tdm_cal.sfd(input,make.plot=TRUE,df=TRUE,wood="Coniferous")

plot(output.data$sfd.pd$sfd[1:1000, ], ylim=c(0,10))
# see estimated uncertainty
lines(output.data$sfd.pd$q025[1:1000, ], lty=1,col="grey")
lines(output.data$sfd.pd$q975[1:1000, ], lty=1,col="grey")
lines(output.data$sfd.pd$sfd[1:1000, ])

sfd_data <- output.data$sfd.dr$sfd