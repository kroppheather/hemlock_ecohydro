library(lubridate)
library(ggplot2)
library(dplyr)
library(zoo)

#### data directory ----
# parent directory
dirUser <- 1

dirData <- c("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/kirkland_ecohydro",
             "G:/My Drive/research/projects/kirkland_ecohydro")

# sap flow
sapRaw <- read.csv(paste0(dirData[dirUser],"/sapflow/09_08_2022/Sapflow_TableDT.dat"),
                   header=FALSE,skip=4,na.strings=c("NAN"))

canopyInv <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/kirkland_ecohydro/HCEF forest inventory data.csv")

HemCanopy <- canopyInv %>%
  filter(Plot == "RG25")
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
    
    sapcalc[i] <- (pi*(((sensors$sd.cm[i])+(Htwd/2))^2))-(pi*((Htwd/2)^2))
    
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

# correction calcs for sap flow based on Clearwater


#a = proportion of probe in sapwood and b=1-a

sensors$a <- ifelse(sensors$sd.cm >= 3,1,
                    sensors$sd.cm/3)

sensors$b <- 1 - sensors$a


#### calculate stand leaf and sapwood area  ----
HemDBH <- HemCanopy %>%
  filter(Species == "TSCA")
HemDBH$LA_tree_m2 <- exp((1.542*log(HemDBH$DBH.cm))-0.274)
HemDBH$SA_cm2 <- exp(-1.192 + (2.010*log(HemDBH$DBH.cm)))
HemDBH$sap.m2 <- 0.0001*HemDBH$SA_cm2
HemSt_SA <- sum(HemDBH$sap.m2)/ (pi*(15^2))
HemSt_LA <- sum(HemDBH$LA_tree_m2)/ (pi*(15^2))

BassDBH <- HemCanopy %>%
  filter(Species == "TIAM")
#basswood projected leaf area
BassDBH$biomass_kg = exp(-4.25 + 1.79*(log(BassDBH$DBH.cm)))
#conversion from Ewers et al SLA averaged over two years (in m2/kg)
BassDBH$LA_tree_m2 <- ((34.8+32.4)/2)*BassDBH$biomass_kg

# calculate sapwood area for each tree

BassDBH$bark <- (BassDBH$DBH.cm*0.0326) - 0.1708
BassDBH$sd.cm <- -0.7783 + (0.24546*BassDBH$DBH.cm)

BassDBH$Htwd <- BassDBH$DBH.cm  - (BassDBH$sd.cm*2) - (BassDBH$bark*2)

#calculate sapwood area

BassDBH$sap.cm2 <- (pi*(((BassDBH$sd.cm) +(BassDBH$Htwd /2))^2))-(pi*((BassDBH$Htwd /2)^2))
BassDBH$sap.m2 <- 0.0001*BassDBH$sap.cm2
BassSt_SA <- sum(BassDBH$sap.m2)/ (pi*(15^2))
BassSt_LA <- sum(BassDBH$LA_tree_m2)/ (pi*(15^2))




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


# quantile filter: removes large spikes in change in temperature that are 
# greater than 1% the quantile change for all sensors

for(i in 3:18){
  tabledt[,i] <- ifelse(tabledt[,i] <= 5 | tabledt[,i] >= 13, NA, tabledt[,i])
}

tableChange <- matrix(rep(NA,nrow(tabledt)*ncol(tabledt)), ncol=ncol(tabledt))
for(i in 3:18){
  for(j in 2:nrow(tableChange)){
    tableChange[j,i] <-  abs(tabledt[j,i] - tabledt[(j-1),i])
  }
}

tableChangeL <- list()
for(i in 3:18){
  tableChangeL[[i]] <- quantile(tableChange[,i], probs=seq(0,1,by=0.01),na.rm=TRUE)
}
# create a flag variable
changeFlag <- matrix(rep(NA,nrow(tabledt)*ncol(tabledt)), ncol=ncol(tabledt))
for(i in 3:18){
  for(j in 2:nrow(tableChange)){
    changeFlag[j,i] <-  ifelse(tableChange[j,i] >= 0.5,NA,1)
  }
} 
# multiple dt by flag so that flagged values are given a NA
tabledtF <- tabledt
for(i in 3:18){
  for(j in 2:nrow(tableChange)){
    tabledtF[j,i] <- changeFlag[j,i]*tabledtF[j,i]
  }
}

# organize dT into a data frame

plot(tabledt$DD, tabledtF[,3], type="l")


dtAll <- data.frame(date= rep(tabledtF$date, times = 16),
                    doy = rep(tabledtF$doy, times = 16),
                    hour = rep(tabledtF$hour, times = 16),
                    DD = rep(tabledtF$DD, times = 16),
                    sensor = rep(seq(1,16), each = nrow(tabledt)),
                    dT = c(tabledtF[,3],
                           tabledtF[,4],
                           tabledtF[,5],
                           tabledtF[,6],
                           tabledtF[,7],
                           tabledtF[,8],
                           tabledtF[,9],
                           tabledtF[,10],
                           tabledtF[,11],
                           tabledtF[,12],
                           tabledtF[,13],
                           tabledtF[,14],
                           tabledtF[,15],
                           tabledtF[,16],
                           tabledtF[,17],
                           tabledtF[,18]))


weather$dateF <- mdy_hms(weather$Date)
weather$doy <- yday(weather$dateF)
weather$hour <- hour(weather$dateF)+(minute(weather$dateF)/60)
weather$DD <- weather$doy + (weather$hour/24)
weatherVPD <- data.frame(doy=weather$doy,
                         hour=weather$hour,
                         DD = weather$DD,
                         VPD=weather$VPD)

ggplot(dtAll, aes(DD, dT, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

## remove abnormal sensor measurements that would occur from maintaince or
## issues around rainfall


quantile(dtAll$dT,probs=seq(0,1,by=0.01), na.rm=TRUE)


ggplot(dtAll, aes(DD, dT, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

ggplot( dtAll %>%
          filter(sensor == 12), aes(DD, dT))+
  geom_point()+
  geom_line()


#### sap flow calculations ----

#join sensor info into table dt
# make the cut off midnight 
dtAll$doy7 <- ifelse(dtAll$hour >= 20,dtAll$doy+1,dtAll$doy )
night <- dtAll[dtAll$hour <= 7 | dtAll$hour >= 20,]



#filter night so maximum in day and sensor is provided
maxnight1 <- night %>%
  group_by(sensor, doy7) %>%
  filter(dT == max(dT),na.rm=TRUE)
#remove duplicate maximums that occur for longer than 15 min
#just take earliest measurement
maxnight <- maxnight1  %>%
  group_by(sensor, doy7) %>%
  filter(hour == min(hour),na.rm=TRUE)



#isolate max and join back into table
maxJoin <- data.frame(sensor=maxnight$sensor,
                      doy7=maxnight$doy7,
                      maxDT = maxnight$dT)

ggplot(maxJoin, aes(doy7, maxDT, color = as.factor(sensor)))+
  geom_line()+
  geom_point()



ggplot(maxJoin%>%filter(sensor == 5), aes(doy7, maxDT, color = as.factor(sensor)))+
  geom_line()+
  geom_point()


ggplot(maxJoin%>%filter(sensor == 6), aes(doy7, maxDT, color = as.factor(sensor)))+
  geom_line()+
  geom_point()
ggplot(maxJoin%>%filter(sensor == 10), aes(doy7, maxDT, color = as.factor(sensor)))+
  geom_line()+
  geom_point()



dayAllDF <- data.frame(sensor = rep(unique(maxJoin$sensor), each=length(seq(166,250))),
                       doy7 = rep(seq(166,250), times=length(unique(maxJoin$sensor))))

maxAllDays <- left_join(dayAllDF, maxJoin, by=c("sensor", "doy7"))



ggplot(maxAllDays, aes(doy7, maxDT, color=as.factor(sensor)))+
  geom_point()

#join backinto tabledt
dtCalct1 <- left_join(dtAll, maxAllDays, by=c("sensor","doy7"))
#join sensor info
dtCalc <- left_join(dtCalct1 , sensors, by=c("sensor"="Sensor.Number"))

#from clearwater

#sap velocity m s-1 (v)
#v = 0.000119*k^1.231

#K= (dTmax - dT)/dT if sensor is fully within sapwood

#otherwise correction is:
#dt sap = (dT - b* Dtmax)/a

# velo is refered to  as Js in Ewers it is m3 of water per m-2 of sap flow per s
# this can reduce in dimension to m per s
dtCalc$dTCor <- (dtCalc$dT - (dtCalc$b * dtCalc$maxDT))/dtCalc$a
# any corrected deviation above max temp 
# is equivalent to the maximum temp and set flow to zero
dtCalc$K <- ifelse(dtCalc$dTCor > dtCalc$maxDT, 0,
                   (dtCalc$maxDT - dtCalc$dTCor)/dtCalc$dTCor)

ggplot(dtCalc %>% filter(sensor == 15), aes(DD, K))+
  geom_point()


dtCalc$velo <- 0.000119*(dtCalc$K^1.231)

ggplot(dtCalc, aes(DD, velo, color=Tree.Type))+
  geom_point()

ggplot(dtCalc %>% filter(sensor == 12), aes(DD, velo, color=Tree.Type))+
  geom_point()

#separate types
hemlockT <- dtCalc[dtCalc$Tree.Type == "Hemlock",]
basswoodT <- dtCalc[dtCalc$Tree.Type == "Basswood",]

ggplot(hemlockT, aes(DD, velo, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

ggplot(basswoodT, aes(DD, velo, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

# check Quantiles for outliers
hemlockQ <- quantile(hemlockT$velo,probs = seq(0,1,by=0.01),na.rm=TRUE)

basswoodQ <- quantile(basswoodT$velo,probs = seq(0,1,by=0.01),na.rm=TRUE)

#  no weird spikes
basswood <-basswoodT 
hemlock <-hemlockT 

ggplot(hemlock, aes(DD, velo, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

ggplot(hemlock%>% filter(doy>=169 & doy<=175), aes(DD, velo, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

ggplot(basswood, aes(DD, velo, color=as.factor(sensor)))+
  geom_point()+
  geom_line()



#### radial correction ----

#############
#compare N & S sensors for hemlock
# this tree has abnormal dT fluctuations and flow. Omitted from analysis
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

treeDirHem <- na.omit(rbind(treeD2,treeD3))
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

# remove trees with issues
# that include irregular changes in dT and
# high nocturnal flow and erratic patterns that are not diurnal
hemlock.filter <- hemlock %>%
  filter(Tree.Number != 3 & Tree.Number != 5)


hemlock.tree <- hemlock.filter %>%
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



#### El calculations   ----
# for comparison of flow and E  from Ewers
# Ewers equation looks for Js (velo) in Kg m-2 s-1
# 1 m3 is 1000 kg
sapFlow$Js <- sapFlow$velo.cor * 1000
# look at Js in grams per m2 per s
ggplot(sapFlow, aes(DD,Js*1000, color=Tree.Type))+
  geom_point()



#Calculate El
# El = Js * Sa/Sl
# Kg m-2 leaf s-1 (also = L m-2 leaf s-1)


sapFlow$El <- sapFlow$Js *(sapFlow$sap.aream2/sapFlow$LA.m2)

sapFlow$hour1 <- floor(sapFlow$hour)

sapFlowNN <- sapFlow %>%
  filter(is.na(El) == FALSE)
sapFlowNN$species <- tolower(sapFlowNN$Tree.Type)
# sap flow per hour
sapFlowNN$El.hr <- sapFlowNN$El*60*60

# hourly table

tree.hour <- sapFlowNN %>%
  group_by(species, Tree.Number, doy, hour1) %>%
  summarise(Jst = mean(Js, na.rm=TRUE), # in kg
            sd.Jst = sd(Js, na.rm=TRUE),
            Elt = mean(El, na.rm=TRUE),
            sd.Elt = sd(El, na.rm=TRUE),
            El.hrtt = mean(El.hr, na.rm=TRUE),
            sd.Elhr = sd(El.hr, na.rm=TRUE),
            n_t= length(na.omit(Js))) %>%
  filter(n_t >=3)


# gap fill separate by species


hemlock_hour <- tree.hour %>%
  filter(species == "hemlock")

basswood_hour <- tree.hour %>%
  filter(species == "basswood")

doyAllHemlock <- data.frame(doy=rep(rep(seq(167,250), each=24),
                                    times=length(unique(hemlock_hour$Tree.Number))),
                            hour=rep(rep(seq(0,23), times= length(seq(167,250))),
                                     times=length(unique(hemlock_hour$Tree.Number))),
                            Tree.Number= rep(unique(hemlock_hour$Tree.Number),
                                             each=length(seq(167,250))*24))

doyAllBasswood <- data.frame(doy=rep(rep(seq(167,250), each=24),
                                     times=length(unique(basswood_hour$Tree.Number))),
                             hour=rep(rep(seq(0,23), times= length(seq(167,250))),
                                      times=length(unique(basswood_hour$Tree.Number))),
                             Tree.Number= rep(unique(basswood_hour$Tree.Number),
                                              each=length(seq(167,250))*24))


basswoodALL <- left_join(doyAllBasswood, basswood_hour,  by=c("doy","hour"="hour1", "Tree.Number"))
hemlockALL <- left_join(doyAllHemlock, hemlock_hour,  by=c("doy","hour"="hour1", "Tree.Number"))

hemlockALL$date <- ymd_hm(paste0(as.Date(hemlockALL$doy, origin="2021-12-31"),
                                 " ", hemlockALL$hour,":00"))

basswoodALL$date <- ymd_hm(paste0(as.Date(basswoodALL$doy, origin="2021-12-31"),
                                  " ", basswoodALL$hour,":00"))


sensorB <- unique(basswoodALL$Tree.Number)
bass_TreeL <- list()
bass_gapZ <- list()
bass_gap <- list()
bass_gapDF <- list()
for(i in 1:length(sensorB)){
  bass_TreeL[[i]] <- basswoodALL %>% filter(Tree.Number == sensorB[i])
  bass_gapZ[[i]] <- zoo(bass_TreeL[[i]]$Elt, bass_TreeL[[i]]$date)
  bass_gap[[i]] <- na.approx(bass_gapZ[[i]], maxgap=3, na.rm=FALSE) 
  bass_gapDF[[i]] <- fortify(bass_gap[[i]])
  bass_gapDF[[i]]$Tree.Number <- rep(sensorB[i], nrow(bass_gapDF[[i]]))
  
}


bass_gapf <- do.call("rbind", bass_gapDF)
bass_gapf$doy <- yday(bass_gapf$Index)
bass_gapf$hour <- hour(bass_gapf$Index)
names(bass_gapf) <- c("date", "Elt","Tree.Number","doy","hour")
# add sensor info back in
Nsensors <- sensors %>% filter(Direction == "N")

bass_gapf <- left_join(bass_gapf, Nsensors, by=c("Tree.Number"))

bass_gapf <- do.call("rbind", bass_gapDF)
bass_gapf$doy <- yday(bass_gapf$Index)
bass_gapf$hour <- hour(bass_gapf$Index)
names(bass_gapf) <- c("date", "Elt","Tree.Number","doy","hour")
# add sensor info back in
Nsensors <- sensors %>% filter(Direction == "N")

bass_gapf <- left_join(bass_gapf, Nsensors, by=c("Tree.Number"))
bass_gapf$species <- rep("basswood", nrow(bass_gapf))


sensorH <- unique(hemlockALL$Tree.Number)
hem_TreeL <- list()
hem_gapZ <- list()
hem_gap <- list()
hem_gapDF <- list()
for(i in 1:length(sensorH)){
  hem_TreeL[[i]] <- hemlockALL %>% filter(Tree.Number == sensorH[i])
  hem_gapZ[[i]] <- zoo(hem_TreeL[[i]]$Elt, hem_TreeL[[i]]$date)
  hem_gap[[i]] <- na.approx(hem_gapZ[[i]], maxgap=3, na.rm=FALSE) 
  hem_gapDF[[i]] <- fortify(hem_gap[[i]])
  hem_gapDF[[i]]$Tree.Number <- rep(sensorH[i], nrow(hem_gapDF[[i]]))
  
}


hem_gapf <- do.call("rbind", hem_gapDF)
hem_gapf$doy <- yday(hem_gapf$Index)
hem_gapf$hour <- hour(hem_gapf$Index)
names(hem_gapf) <- c("date", "Elt","Tree.Number","doy","hour")
# add sensor info back in

hem_gapf <- left_join(hem_gapf, Nsensors, by=c("Tree.Number"))
hem_gapf$species <- rep("hemlock", nrow(hem_gapf))

gapf_El <-rbind(hem_gapf, bass_gapf)
gapf_El$El.hrt <- gapf_El$Elt*60*60



ggplot(sapFlow, aes(DD, Js, color=Tree.Number))+
  geom_point()

##############################
#### Summary tables    ----


ggplot(tree.hour, aes(doy+(hour1/24),El.hrtt, color=as.factor(Tree.Number)))+
  geom_line()

# hourly averages across trees with no gap filling
sapflow.hour <- tree.hour %>%
  group_by(doy, hour1, species) %>%
  summarise(Js = mean(Jst, na.rm=TRUE),
            sd_Js = sd(Jst, na.rm=TRUE),
            n_Js = length(na.omit(Jst)), 
            El = mean(Elt, na.rm=TRUE),
            sd_El = sd(Elt, na.rm=TRUE),
            El_hr = mean(El.hrtt, na.rm=TRUE),
            sd_Elhr = sd(El.hrtt, na.rm=TRUE))%>%
  filter(n_Js >= 3) 


# max js
sapflow.max.tree <- tree.hour %>%
  group_by(species, Tree.Number, doy) %>%
  summarise(maxJst= max(Jst, na.rm=TRUE),
            n_t= length(na.omit(Jst)))


sapflow.max <- sapflow.max.tree %>%
  filter(n_t >= 22) %>%
  group_by(species, doy) %>%
  summarise(max_Js = mean(maxJst),
            sd_mJs = sd(maxJst),
            n_max= n()) %>%
  filter(n_max >=3)

sapflow.max$se_mJs = sapflow.max$sd_mJs/sqrt(sapflow.max$n_max)

ggplot(sapflow.max, aes(doy, max_Js, color=species))+
  geom_point()

# daily totals

# covert JS to hour to sum to sapflow to day
Tot.tree.L.day <- gapf_El %>%
  group_by(Tree.Number, doy, species) %>%
  summarise(Tot_El_day = sum(El.hrt, na.rm=TRUE),
            Tot_n_day = length(na.omit(El.hrt)))%>%
  filter(Tot_n_day ==24)

# Kg m-2 leaf day day
T.L.day <- Tot.tree.L.day %>%
  group_by( doy, species) %>%
  summarise(El_day = mean(Tot_El_day), # per tree
            sd_El = sd(Tot_El_day),
            n_plant = length(na.omit(Tot_El_day)))%>% 
  filter(n_plant >= 3)

ggplot(T.L.day, aes(doy, El_day, color=species))+
  geom_point()+
  geom_line()

ggplot(sapflow.hour%>%filter(doy==182), aes(hour1, Js, color=species))+
  geom_point()+
  geom_line()

ggplot(sapflow.hour%>%filter(doy==241 ), aes(hour1, Js, color=species))+
  geom_point()+
  geom_line()


rm(list=setdiff(ls(), c("T.L.day","sapflow.hour", "Tot.tree.L.day", "dirScript", "tree.hour", "sensors", "sapflow.max")))

