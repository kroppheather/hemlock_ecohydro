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

ggplot(dtAll, aes(DD, dT, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

## remove abnormal sensor measurements that would occur from maintaince or
## issues around rainfall


quantile(dtAll$dT,probs=seq(0,1,by=0.01), na.rm=TRUE)
# quantile filter: removes 1% outliers
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

#K= (dTmax - dT)/dT if sensor is fully within sapwood

#otherwise correction is:
#dt sap = (dT - b* Dtmax)/a

#a = proportion of probe in sapwood and b=1-a

dtCalc$a <- ifelse(dtCalc$sd.cm >= 3,1,
                   dtCalc$sd.cm/3)

dtCalc$b <- 1 - dtCalc$a
# velo is refered to  as Js in Ewers it is m3 of water per m-2 of sap flow per s
# this can reduce in dimension to m per s
dtCalc$dTCor <- (dtCalc$dT - (dtCalc$b * dtCalc$maxDT))/dtCalc$a
dtCalc$K <- (dtCalc$maxDT - dtCalc$dTCor)/dtCalc$dTCor
dtCalc$velo <- 0.000119*(dtCalc$K^1.231)

ggplot(dtCalc, aes(DD, velo, color=Tree.Type))+
  geom_point()

#separate types
hemlockT <- dtCalc[dtCalc$Tree.Type == "Hemlock",]
basswoodT <- dtCalc[dtCalc$Tree.Type == "Basswood",]

ggplot(hemlockT, aes(DD, velo, color=as.factor(sensor)))+
         geom_point()+
         geom_line()

# filter anything above the 98 percentile
hemlockQ <- quantile(hemlockT$velo,probs = seq(0,1,by=0.01),na.rm=TRUE)

hemlock <- hemlockT %>%
  filter(velo <= hemlockQ[99])

basswoodQ <- quantile(basswoodT$velo,probs = seq(0,1,by=0.01),na.rm=TRUE)


basswood <-basswoodT %>%
  filter(velo <= basswoodQ[99])

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
hemlock.tree$velo.cor <- (hemlock.tree$velo*0.5)+(((hemlock.tree$velo*hemlock.cor[2])+0)*0.5)

basswood.cor <- coefficients(azimB.rel)
basswood.tree$velo.cor <- (basswood.tree$velo*0.5)+(((basswood.tree$velo*basswood.cor[2])+0)*0.5)


## bind trees back together

sapFlow <- rbind(hemlock.tree, basswood.tree)

ggplot(sapFlow, aes(DD, velo.cor, color=as.factor(Tree.Number)))+
  geom_point()

ggplot(sapFlow %>% filter(Tree.Number == 6), aes(DD, velo.cor,))+
  geom_point()

#### El calculations   ----
# for comparison of flow and E  from Ewers
# Ewers equation looks for Js (velo) in Kg m-2 s-1
# 1 m3 is 1000 kg
sapFlow$Js <- sapFlow$velo.cor * 1000
# look at Js in grams per m2 per s
ggplot(sapFlow, aes(DD,Js*1000, color=Tree.Type))+
  geom_point()
# look at Js in grams per cm2 per day
ggplot(sapFlow, aes(DD,Js*1000*0.0001*60*60*24, color=Tree.Type))+
  geom_point()


#Calculate El
# El = Js * Sa/Sl
# Kg m-2 leaf s-1 (also = L m-2 leaf s-1)


sapFlow$El <- sapFlow$Js *(sapFlow$sap.aream2/sapFlow$LA.m2)


# look at El_mm in mm
sapFlow$El_mm <- (((sapFlow$Js/1000) *(sapFlow$sap.aream2/sapFlow$LA.m2))*1000)*60*60*24
ggplot(sapFlow, aes(DD,El_mm, color=Tree.Type))+
  geom_point()
ggplot(sapFlow, aes(DD,El, color=Tree.Type))+
  geom_point()

# sap flow per hour
sapFlow$El.hr <- sapFlow$El*60*60

sapFlow$hour1 <- floor(sapFlow$hour)

sapFlowNN <- sapFlow %>%
  filter(is.na(El) == FALSE)

sapFlowNN$species <- tolower(sapFlowNN$Tree.Type)

##############################
#### Summary tables    ----
# hourly table

tree.hour <- sapFlowNN %>%
  group_by(species, Tree.Number, doy, hour1) %>%
  summarise(Jst = mean(Js, na.rm=TRUE), # in kg
            sd.Jst = sd(Js, na.rm=TRUE),
            Elt = mean(El, na.rm=TRUE),
            sd.Elt = sd(El, na.rm=TRUE),
            El.hrtt = mean(El.hr, na.rm=TRUE),
            sd.Elhr = sd(El.hr, na.rm=TRUE),
            n_t= length(na.omit(Js)))
  
ggplot(tree.hour, aes(doy+(hour1/24),El.hrtt, color=as.factor(Tree.Number)))+
  geom_line()

# hourly averages across trees
sapflow.hour <- tree.hour %>%
  group_by(doy, hour1, species) %>%
  summarise(Js = mean(Jst, na.rm=TRUE),
            sd_Js = sd(Jst, na.rm=TRUE),
            n = length(na.omit(Jst)),
            El = mean(Elt, na.rm=TRUE),
            sd_El = sd(Elt, na.rm=TRUE),
            El_hr = mean(El.hrtt, na.rm=TRUE),
            sd_Elhr = sd(El.hrtt, na.rm=TRUE)) %>%
  filter(n >= 3)


# daily totals


Tot.tree.L.day <- tree.hour %>%
  group_by(Tree.Number, doy, species) %>%
  summarise(Tot_El_day = sum(El.hrtt, na.rm=TRUE),
            Tot_n_day = length(na.omit(El.hrtt))) %>%
  filter(Tot_n_day >=22)

T.L.day <- Tot.tree.L.day %>%
  group_by( doy, species) %>%
  summarise(El_day = mean(Tot_El_day), # per tree
            sd_El = sd(Tot_El_day),
            n_plant = length(na.omit(Tot_El_day))) %>% 
  filter(n_plant >= 3)

ggplot(T.L.day, aes(doy, El_day, color=species))+
  geom_point()


rm(list=setdiff(ls(), c("T.L.day","sapflow.hour", "Tot.tree.L.day", "dirScript")))

   