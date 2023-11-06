# organize soil and weather data

library(lubridate)
library(ggplot2)
library(dplyr)

dirUser <- 2

dirData <- c("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/kirkland_ecohydro",
             "E:/Google Drive/research/projects/kirkland_ecohydro")


weather <- read.csv(paste0(dirData[dirUser],"/weather/z6-10463(z6-10463)-1694459136/z6-10463(z6-10463)-Configuration 1-1694459136.3651896.csv"),
                    skip=3, header=FALSE)

colnames(weather) <- c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                          "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                          "SensorTemp","VPD","BatPct","BatVolt","RefPr","LogTemp")
# tomst sensors for hemlock
tomst1 <- read.csv(paste0(dirData[dirUser],"/tomst_09_23/data_94214743_2023_09_20_0.csv"),
                   sep=";", header=FALSE)[,1:9]
tomst2 <- read.csv(paste0(dirData[dirUser],"/tomst_09_23/data_94214744_2023_09_20_0.csv"),
                   sep=";", header=FALSE)[,1:9]
tomst3 <- read.csv(paste0(dirData[dirUser],"/tomst_09_23/data_94236483_2023_09_20_0.csv"),
                   sep=";", header=FALSE )[,1:9]


#### set up dates for weather ----
weather$dateF <- mdy_hms(weather$Date)
weather$doy <- yday(weather$dateF)
weather$hour <- hour(weather$dateF)
weather$year <- year(weather$dateF)
#### organize soil data

#tms temps:  -6, +2 and +15cm
TMScols <- c("record","date","tz","Tm6","T2","T15","SM","shake","errFlag")
colnames(tomst1) <- TMScols
colnames(tomst2) <- TMScols
colnames(tomst3) <- TMScols

# replace decimal commas
tomst1$Tm6 <- as.numeric(gsub("\\,","\\.",tomst1$Tm6))
tomst1$T2 <- as.numeric(gsub("\\,","\\.",tomst1$T2))
tomst1$T15 <- as.numeric(gsub("\\,","\\.",tomst1$T15))

tomst2$Tm6 <- as.numeric(gsub("\\,","\\.",tomst2$Tm6))
tomst2$T2 <- as.numeric(gsub("\\,","\\.",tomst2$T2))
tomst2$T15 <- as.numeric(gsub("\\,","\\.",tomst2$T15))

tomst3$Tm6 <- as.numeric(gsub("\\,","\\.",tomst3$Tm6))
tomst3$T2 <- as.numeric(gsub("\\,","\\.",tomst3$T2))
tomst3$T15 <- as.numeric(gsub("\\,","\\.",tomst3$T15))

tomst1$sensor <- rep(1,nrow(tomst1))
tomst2$sensor <- rep(2,nrow(tomst2))
tomst3$sensor <- rep(3,nrow(tomst3))

tomst <- rbind(tomst1, tomst2, tomst3)

tomst$dateF <- ymd_hm(tomst$date)
tomst$estD <- with_tz(tomst$dateF,tzone="America/New_York" )

# soils are silty loam. Calculate moisture based on texture

tomst$SM.cor <- (-0.00000002*(tomst$SM^2)) + (0.0003*tomst$SM) -0.2062
tomst$year <- year(tomst$estD)
tomst$doy <-  yday(tomst$estD)
tomst$hour <- hour(tomst$estD)


# average over the hour

soilAveH <- tomst %>%
  group_by(sensor, year,doy,hour) %>%
  summarise(AH_soil_temp = mean(Tm6, na.rm = TRUE),
            AH_sd_temp = sd(Tm6, na.rm = TRUE),
            AH_n_temp = length(na.omit(Tm6)),
            AH_SM = mean(SM.cor, na.rm=TRUE),
            AH_sd_SM = sd(SM.cor, na.rm=TRUE),
            AH_n_SM = length(na.omit(SM.cor))) %>%
    filter(doy >= 160 & doy <= 251 &  year == 2022)

# average over sensors
soilHourly<- soilAveH %>%
  group_by(year,doy,hour) %>%
  summarise(soil_temp = mean(AH_soil_temp, na.rm = TRUE),
            sd_temp = sd(AH_soil_temp, na.rm = TRUE),
            n_temp = length(na.omit(AH_soil_temp)),
            SM = mean(AH_SM, na.rm=TRUE),
            sd_SM = sd(AH_SM, na.rm=TRUE),
            n_SM = length(na.omit(AH_SM))) %>%
  filter(n_SM >=3)

soilHourly$DD <- soilHourly$doy + (soilHourly$hour/24)


soilDaily <- soilHourly %>%
  group_by(year,doy) %>%
  summarise(s_temp = mean(soil_temp, na.rm = TRUE),
            sd_st = sd(soil_temp, na.rm = TRUE),
            n_t = length(na.omit(soil_temp)),
            SWC = mean(SM, na.rm=TRUE),
            sd_SWC = sd(SM, na.rm=TRUE),
            n_SWC = length(na.omit(SM)))

weatherHourly <- weather %>%
  group_by(year, doy, hour) %>%
  summarise(Precip = sum(Precip, na.rm=TRUE),
            n_Precip = length(na.omit(Precip)),
            VPD_hr = mean(VPD, na.rm=TRUE),
            S_Rad = mean(SolRad, na.rm=TRUE),
            Air_temp = mean(AirTemp, na.rm=TRUE),
            AtmosPr=  mean(AtmosPr, na.rm=TRUE))%>%
  filter(doy >= 158 & doy <= 251 &  year == 2022)

weatherHourly$DD <- weatherHourly$doy + (weatherHourly$hour/24)

weatherDaily <- weatherHourly %>%
  group_by(year, doy) %>%
  summarise(Prec = sum(Precip, na.rm=TRUE),
            n_Prec = length(na.omit(Precip)),
            aveVPD = mean(VPD_hr, na.rm=TRUE),
            maxVPD = max(VPD_hr, na.rm=TRUE),
            max_SW = max(S_Rad, na.rm=TRUE),
            AirT = mean(Air_temp, na.rm=TRUE),
            maxAirT = max(Air_temp, na.rm=TRUE),
            minAirT= min(Air_temp, na.rm=TRUE))
  


rm(list=setdiff(ls(), c("soilDaily","soilHourly", "weatherDaily", "weatherHourly","T.L.day","sapflow.hour", "Tot.tree.L.day","dirScript")))