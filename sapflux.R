library(lubridate)
library(ggplot2)
library(dplyr)

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
########## Tree allometry ---


## sapwood area

# hemlock tree
# sapwood area measurements are from 
plot(hemlockmeas$DBH.cm, log(hemlockmeas$SapwoodArea.cm2))
sapareaHem <- lm(log(hemlockmeas$SapwoodArea.cm2) ~ log(hemlockmeas$DBH.cm))
abline(sapareaHem)
summary(sapareaHem)






hemlock.tree$sap.areacm2 <- exp(-1.192 + (2.010*log(hemlock.tree$DBH..cm.)))
#convert sap area to m2
hemlock.tree$sap.aream2 <- 0.0001*hemlock.tree$sap.areacm2

# basswood

#calculate heartwood

basswood.tree$bark <- (basswood.tree$DBH..cm.*0.0326) - 0.1708

basswood.tree$Htwd <- basswood.tree$DBH..cm.  - (basswood.tree$sd.cm*2) - (basswood.tree$bark*2)



#calculate sapwood area

basswood.tree$sap.areacm2 <- (pi*(((basswood.tree$sd.cm/2)+(basswood.tree$Htwd/2))^2))-(pi*((basswood.tree$Htwd/2)^2))
basswood.tree$sap.aream2 <-  0.0001*basswood.tree$sap.areacm2


#check relationship
# lm.log<- lm(log(basswoodLA$LA.m2) ~ log(basswoodLA$DBH.cm))
# summary(lm.log)
# plot(basswoodLA$DBH.cm, basswoodLA$LA.m2)
# plot(log(basswoodLA$DBH.cm), log(basswoodLA$LA.m2))
#regression log(LA (m2)) = -1.058 + 1.828 * log(dbh.cm)

#estimate leaf area in m2

#crown = 1.6961 + 0.4233(DBH)

#basswood leaf area to the best of our ability
#mean basswood weight = 22.1324289 g/m2
#1/slw = 0.04518257

# data from supplement in dettman mcfarlane
blm <- basswoodlm %>%
  filter(SPECIES =="Tilia americana")
plot(log(blm$DBH_CM), log(blm$LEAF_DRY_MASS_KG))

b.mod <- lm(log(blm$LEAF_DRY_MASS_KG)~log(blm$DBH_CM))
summary(b.mod)

basswood.tree$biomass.kg = exp(-4.25 + 1.79*(log(basswood.tree$DBH..cm.)))


#conversion from Ewers et al SLA averaged over two years (in m2/kg)
basswood.tree$LA.m2 <- ((34.8+32.4)/2)*basswood.tree$biomass.kg

#leaf area in m2 for hemlock, from Ford and Vose 2007
hemlock.tree$LA.m2 <- exp((1.542*log(hemlock.tree$DBH..cm.))-0.274)

# test

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

#organize data for easier calculations
tabledt <- datSap


dtAll <- data.frame(date= rep(tabledt$date, times = 16),
                    doy = rep(tabledt$doy, times = 16),
                    hour = rep(tabledt$hour, times = 16),
                    DD = rep(tabledt$DD, times = 16),
                    sensor = rep(seq(1,16), each = nrow(tabledt)),
                    dT = c(tabledt[,3],
                           tabledt[,4],
                           tabledt[,5],
                           tabledt[,6],
                           tabledt[,7],
                           tabledt[,8],
                           tabledt[,9],
                           tabledt[,10],
                           tabledt[,11],
                           tabledt[,12],
                           tabledt[,13],
                           tabledt[,14],
                           tabledt[,15],
                           tabledt[,16],
                           tabledt[,17],
                           tabledt[,18]))

## remove abnormal sensor measurements that would occur from maintaince or
## issues around rainfall
dtAll <- dtAll %>%
  filter(dT >=4 & dT <= 11)

dtPlot <- dtAll %>%
  filter(sensor == 5)
ggplot(dtAll, aes(DD, dT, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

ggplot(dtPlot, aes(DD, dT))+
  geom_point()+
  geom_line()


#### sap flow calculations ----

#join sensor info into table dt
#make a doy that contains the same night
#so new day actually starts at 5 am not midnight
dtAll$doy5 <- ifelse(dtAll$hour < 5, dtAll$doy-1,dtAll$doy)

night <- dtAll[dtAll$hour < 5|dtAll$hour >= 22,]

#filter night so maximum in day and sensor is provided
maxnight1 <- night %>%
  group_by(sensor, doy5) %>%
  filter(dT == max(dT),na.rm=TRUE)
#remove duplicate maximums that occur for longer than 15 min
#just take earliest measurement
maxnight <- maxnight1  %>%
  group_by(sensor, doy5) %>%
  filter(hour == min(hour),na.rm=TRUE)

#ggplot(maxnight, aes(doy5,dT, color=sensor))+
#geom_point()
#isolate max and join back into table
maxJoin <- data.frame(sensor=maxnight$sensor,
                      doy5=maxnight$doy5,
                      maxDT = maxnight$dT)

ggplot(maxJoin, aes(doy5, maxDT, color = as.factor(sensor)))+
  geom_line()+
  geom_point()


#join backinto tabledt
dtCalct1 <- left_join(dtAll, maxJoin, by=c("sensor","doy5"))
#join sensor info
dtCalc <- left_join(dtCalct1 , sensors, by=c("sensor"="Sensor.Number"))

#from clearwater

#sap velocity m s-1 (v)
#v = 0.000119*k^1.231
#flow is F (L s-1) = v* A (m2, sapwood area)

#K= (dTmax - dT)/dT if sensor is fully within sapwood

#otherwise correction is:
#dt sap = (dT - b* Dtmax)/a

#a = proportion of probe in sapwood and b=1-a

dtCalc$a <- ifelse(dtCalc$sd.cm >= 3,1,
                   dtCalc$sd.cm/3)

dtCalc$b <- 1 - dtCalc$a

dtCalc$dTCor <- (dtCalc$dT - (dtCalc$b * dtCalc$maxDT))/dtCalc$a
dtCalc$K <- (dtCalc$maxDT - dtCalc$dTCor)/dtCalc$dTCor
dtCalc$velo <- 0.000119*(dtCalc$K^1.231)


#separate types
hemlockT <- dtCalc[dtCalc$Tree.Type == "Hemlock",]
basswoodT <- dtCalc[dtCalc$Tree.Type == "Basswood",]

ggplot(hemlockT, aes(DD, velo, color=as.factor(sensor)))+
         geom_point()+
         geom_line()

# filter anything above the 99 percentile
hemlockQ <- quantile(hemlockT$velo,probs = seq(0,1,by=0.01),na.rm=TRUE)

hemlock <- hemlockT %>%
  filter(velo <= hemlockQ[100])

basswoodQ <- quantile(basswoodT$velo,probs = seq(0,1,by=0.01),na.rm=TRUE)


basswood <-basswoodT %>%
  filter(velo <= basswoodQ[100])

ggplot(hemlock, aes(DD, velo, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

ggplot(basswood, aes(DD, velo, color=as.factor(sensor)))+
  geom_point()+
  geom_line()



#### radial correction ----

#############
#compare N & S sensors for hemlock
sens5 <- na.omit(data.frame(date = hemlock$date[hemlock$sensor == 5],
                            veloN = hemlock$velo[hemlock$sensor == 5]))

sens6 <- na.omit(data.frame(date = hemlock$date[hemlock$sensor == 6],
                            veloS = hemlock$velo[hemlock$sensor == 6]))

treeD1 <- inner_join(sens5,sens6, by="date")

plot(treeD1$veloN, treeD1$veloS)
#compare N & S sensors for hemlock
sens12 <- na.omit(data.frame(date = hemlock$date[hemlock$sensor == 12],
                             veloN = hemlock$velo[hemlock$sensor == 12]))

sens11 <- na.omit(data.frame(date = hemlock$date[hemlock$sensor == 11],
                             veloS = hemlock$velo[hemlock$sensor == 11]))

treeD2 <- inner_join(sens12,sens11, by="date")

sens15 <- na.omit(data.frame(date = hemlock$date[hemlock$sensor == 15],
                             veloN = hemlock$velo[hemlock$sensor == 15]))

sens14 <- na.omit(data.frame(date = hemlock$date[hemlock$sensor == 14],
                             veloS = hemlock$velo[hemlock$sensor == 14]))

treeD3 <- inner_join(sens15,sens14, by="date")

treeDirHem <- na.omit(rbind(treeD1,treeD2,treeD3))
#check relationship
azim.rel <- lm(treeDirHem$veloS ~ treeDirHem$veloN)
summary(azim.rel)

ggplot(treeDirHem, aes(veloN,veloS))+
  geom_point()+
  geom_abline()

# check basswood


sens1 <- na.omit(data.frame(date = basswood$date[basswood$sensor == 1],
                            veloN = basswood$velo[basswood$sensor == 1]))

sens2 <- na.omit(data.frame(date = basswood$date[basswood$sensor == 2],
                            veloS = basswood$velo[basswood$sensor == 2]))

treeB1 <- inner_join(sens1,sens2, by="date")

sens3 <- na.omit(data.frame(date = basswood$date[basswood$sensor == 3],
                            veloN = basswood$velo[basswood$sensor == 3]))

sens4 <- na.omit(data.frame(date = basswood$date[basswood$sensor == 4],
                            veloS = basswood$velo[basswood$sensor == 4]))

treeB2 <- inner_join(sens3,sens4, by="date")

sens8 <- na.omit(data.frame(date = basswood$date[basswood$sensor == 8],
                            veloN = basswood$velo[basswood$sensor == 8]))

sens9 <- na.omit(data.frame(date = basswood$date[basswood$sensor == 9],
                            veloS = basswood$velo[basswood$sensor == 9]))

treeB3 <- inner_join(sens8,sens9, by="date")

treeBDir <- rbind(treeB1,treeB2, treeB3)

azimB.rel <- lm(treeBDir$veloS ~ treeBDir$veloN)
summary(azimB.rel)

ggplot(treeBDir, aes(veloN,veloS))+
  geom_point()+
  geom_abline()



hemlock.tree <- hemlock %>%
  filter(Direction == "N")

basswood.tree <- basswood %>%
  filter(Direction == "N")

# running radial correction
hemlock.cor <- coefficients(azim.rel)
hemlock.tree$velo.cor <- (hemlock.tree$velo*0.5)+(((hemlock.tree$velo*hemlock.cor[2])+hemlock.cor[1])*0.5)

basswood.cor <- coefficients(azimB.rel)
basswood.tree$velo.cor <- (basswood.tree$velo*0.5)+(((basswood.tree$velo*basswood.cor[2])+basswood.cor[1])*0.5)

#### Canopy calculations   ----

## sapwood area

# hemlock tree
plot(log(hemlockmeas$DBH.cm), log(hemlockmeas$SapwoodArea.cm2))
sapareaHem <- lm(log(hemlockmeas$SapwoodArea.cm2) ~ log(hemlockmeas$DBH.cm))
abline(sapareaHem)
summary(sapareaHem)

hemlock.tree$sap.areacm2 <- exp(-1.192 + (2.010*log(hemlock.tree$DBH..cm.)))
#convert sap area to m2
hemlock.tree$sap.aream2 <- 0.0001*hemlock.tree$sap.areacm2

# basswood

#calculate heartwood

basswood.tree$bark <- (basswood.tree$DBH..cm.*0.0326) - 0.1708

basswood.tree$Htwd <- basswood.tree$DBH..cm.  - (basswood.tree$sd.cm*2) - (basswood.tree$bark*2)



#calculate sapwood area

basswood.tree$sap.areacm2 <- (pi*(((basswood.tree$sd.cm/2)+(basswood.tree$Htwd/2))^2))-(pi*((basswood.tree$Htwd/2)^2))
basswood.tree$sap.aream2 <-  0.0001*basswood.tree$sap.areacm2


#check relationship
# lm.log<- lm(log(basswoodLA$LA.m2) ~ log(basswoodLA$DBH.cm))
# summary(lm.log)
# plot(basswoodLA$DBH.cm, basswoodLA$LA.m2)
# plot(log(basswoodLA$DBH.cm), log(basswoodLA$LA.m2))
#regression log(LA (m2)) = -1.058 + 1.828 * log(dbh.cm)

#estimate leaf area in m2

#crown = 1.6961 + 0.4233(DBH)

#basswood leaf area to the best of our ability
#mean basswood weight = 22.1324289 g/m2
#1/slw = 0.04518257

# data from supplement in dettman mcfarlane
blm <- basswoodlm %>%
      filter(SPECIES =="Tilia americana")
plot(log(blm$DBH_CM), log(blm$LEAF_DRY_MASS_KG))

b.mod <- lm(log(blm$LEAF_DRY_MASS_KG)~log(blm$DBH_CM))
summary(b.mod)

basswood.tree$biomass.kg = exp(-4.25 + 1.79*(log(basswood.tree$DBH..cm.)))


#conversion from Ewers et al SLA averaged over two years (in m2/kg)
basswood.tree$LA.m2 <- ((34.8+32.4)/2)*basswood.tree$biomass.kg

#leaf area in m2 for hemlock, from Ford and Vose 2007
hemlock.tree$LA.m2 <- exp((1.542*log(hemlock.tree$DBH..cm.))-0.274)

# test


#### Flow calculations   ----

#El flow rate according to clearwater and Ewers 
#F(L s-1) =  v(m s-1)* A (m2)

hemlock.tree$Flow.m3.s <- hemlock.tree$velo * hemlock.tree$sap.aream2

basswood.tree$Flow.m3.s <- basswood.tree$velo * basswood.tree$sap.aream2


#convert to L per secton

hemlock.tree$Flow.L.s <- hemlock.tree$Flow.m3.s * 1000

basswood.tree$Flow.L.s <- basswood.tree$Flow.m3.s * 1000

#normalize by canopy leaf area for El
hemlock.tree$Flow.L.m2.s <- hemlock.tree$Flow.L.s /hemlock.tree$LA.m2

basswood.tree$Flow.L.m2.s <- basswood.tree$Flow.L.s /basswood.tree$LA.m2


#summarize total per day for each tree
#remove NA
hemlock.treeNN <- hemlock.tree %>%
  filter(is.na(Flow.L.s)==FALSE)

hemlock.treeNN$hour1 <- floor(hemlock.treeNN$hour)

#summarize total per day for each tree
#remove NA
basswood.treeNN <- basswood.tree %>%
  filter(is.na(Flow.L.s)==FALSE)

basswood.treeNN$hour1 <- floor(basswood.treeNN$hour)

##############################
#### Summary tables    ----


# calculate average hourly T
hemlock.tree.L.hour <- hemlock.treeNN %>%
  group_by(Tree.Number, doy, hour1) %>%
  summarise(L.hr = mean(Flow.L.s*60*60, na.rm=TRUE),
            sd.L.hr = sd(Flow.L.s*60*60, na.rm=TRUE),
            n = length(na.omit(Flow.L.s)),
            L.hr.m2 = mean(Flow.L.m2.s*60*60, na.rm=TRUE),
            sd.L.hr.m2 = sd(Flow.L.m2.s*60*60, na.rm=TRUE),
            n.m2 = length(na.omit(Flow.L.m2.s))) %>%
  filter(n>=3)
hemlock.tree.L.hour$species <- rep("hemlock",nrow(hemlock.tree.L.hour))

basswood.tree.L.hour <- basswood.treeNN %>%
  group_by(Tree.Number, doy, hour1) %>%
  summarise(L.hr = mean(Flow.L.s*60*60, na.rm=TRUE),
            sd.L.hr = sd(Flow.L.s*60*60, na.rm=TRUE),
            n = length(na.omit(Flow.L.s)),
            L.hr.m2 = mean(Flow.L.m2.s*60*60, na.rm=TRUE),
            L.hr.m2 = sd(Flow.L.m2.s*60*60, na.rm=TRUE),
            n.m2 = length(na.omit(Flow.L.m2.s))) %>%
  filter(n>=3) # at least 3 obs present for a tree in a hour

basswood.tree.L.hour$species <- rep("basswood",nrow(basswood.tree.L.hour))

# combine tables
tree.L.hour <- rbind(hemlock.tree.L.hour, basswood.tree.L.hour)


# hourly averages across trees
sapflow.hour <- tree.L.hour %>%
  group_by(doy, hour1, species) %>%
  summarise(T.L.hr = mean(L.hr, na.rm=TRUE),
            T.sd.L.hr = sd(L.hr, na.rm=TRUE),
            T.n = length(na.omit(L.hr)),
            T.L.hr.m2 = mean(L.hr.m2, na.rm=TRUE),
            T.sd.L.hr.m2 = sd(L.hr.m2, na.rm=TRUE),
            T.n.m2 = length(na.omit(L.hr.m2))) %>%
  filter(T.n >= 3)


# daily totals


Tot.tree.L.day <- tree.L.hour %>%
  group_by(Tree.Number, doy, species) %>%
  summarise(Tot.L.day = sum(L.hr, na.rm=TRUE),
            Tot.n.day = length(na.omit(L.hr)),
            Tot.L.day.m2 = sum(L.hr.m2, na.rm=TRUE)) %>%
  filter(Tot.n.day >=22)

T.L.day <- Tot.tree.L.day %>%
  group_by( doy, species) %>%
  summarise(L.day = mean(Tot.L.day), # per tree
            sd.Lday = sd(Tot.L.day),
            n.plant = length(na.omit(Tot.n.day)),
            L.day.m2 = mean(Tot.L.day.m2), # per m2 of leaf area
            sd.day.m2 = sd(Tot.L.day.m2)) %>% 
  filter(n.plant >= 3)


rm(list=setdiff(ls(), c("T.L.day","sapflow.hour", "Tot.tree.L.day", "dirScript")))

   