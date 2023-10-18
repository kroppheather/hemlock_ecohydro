#######################################
## Analysis of Hemlock and Basswood  ##
## sap flux and canopy T with soil   ##
## moisture and weather              ##
#######################################

#### read in libraries ----
library(lubridate)
library(ggplot2)
library(dplyr)

#### set up directories ----
dirUser <- 2

dirScriptAll <- c("/Users/hkropp/Documents/GitHub/hemlock_ecohydro",
               "c:/Users/hkropp/Documents/GitHub/hemlock_ecohydro")

dirScript <- dirScriptAll[2]

#### sapflow data ----
## T.L.day: average daily transpiration by species in 
## liters per day per tree and per m2 of leaf area
## sapflow.hour: average hourly sap flow by species in
## L per hour per tree and per m2 of leaf area
## liters per day per tree and per m2 of leaf area
## Tot.tree.L.day: average daily transpiration by tree in
## liters per day per tree and per m2 of leaf area

source(paste0(dirScript, "/sapflux.r"))

#### soil and weather data ----
# soilDaily and soilHourly give soil moisture and temperature
# weatherDaily and weatherHourly have hourly and daily stats

source(paste0(dirScript, "/soil_weather.r"))

#### explore basic data patterns  ----
# daily data
T.L.dayW <- left_join(weatherDaily, T.L.day, by=c("doy"))
Tc.L.day <- left_join(T.L.dayW , soilDaily, by="doy")

### hourly data
Sap_hemlock <- sapflow.hour %>%
  filter(species == "hemlock")
Sap_basswood <- sapflow.hour %>%
  filter(species == "basswood")

dateAll <- data.frame( doy = rep(seq(160,151), each=24),
                       hour = rep(seq(0,23), times=length(seq(160,151))))

hemDateFill <- full_join(dateAll, Sap_hemlock, by=c("doy","hour"="hour1"))
bassDateFill <- full_join(dateAll, Sap_basswood, by=c("doy","hour"="hour1"))
# join weather
bassWeather <- full_join(weatherHourly, bassDateFill, by=c("doy","hour"))
hemWeather <- full_join(weatherHourly, hemDateFill, by=c("doy","hour"))

#get data frame of historical VPD
VPD_sub <- data.frame(doy = weatherHourly$doy,
                      hour = weatherHourly$hour,
                      VPD = weatherHourly$VPD_hr,
                      SRad = weatherHourly$S_Rad)

# ensure data is complete
VPD_df <- full_join(VPD_sub, dateAll, by=c("doy","hour"))
VPD_df <- VPD_df %>%
  arrange(doy, hour)
# get 1 hour before
VPD_df$VPD_m1 <- c(NA,VPD_df$VPD[1:(length(VPD_df$VPD)-1)])
VPD_df$SRad_m1 <- c(NA, VPD_df$SRad[1:(length(VPD_df$SRad)-1)])
# 2 hours prior
VPD_df$VPD_m2 <- c(NA,NA,VPD_df$VPD[1:(length(VPD_df$VPD)-2)])
VPD_df$SRad_m2 <- c(NA, NA, VPD_df$SRad[1:(length(VPD_df$SRad)-2)])

# 3 hours prior
VPD_df$VPD_m3 <- c(NA,NA,NA,VPD_df$VPD[1:(length(VPD_df$VPD)-3)])
VPD_df$SRad_m3 <- c(NA, NA,NA, VPD_df$SRad[1:(length(VPD_df$SRad)-3)])

# 4 hours prior
VPD_df$VPD_m4 <- c(NA,NA,NA,NA,VPD_df$VPD[1:(length(VPD_df$VPD)-4)])
VPD_df$SRad_m4 <- c(NA, NA,NA, NA, VPD_df$SRad[1:(length(VPD_df$SRad)-4)])

VPD_dfJ <- VPD_df %>%
  select(!c(VPD, SRad))

bassH_all <- left_join(bassWeather, VPD_dfJ, by=c("doy", "hour"))
hemH_all <- left_join(hemWeather, VPD_dfJ, by=c("doy", "hour"))

sap_all <- rbind(bassH_all, hemH_all)

# join in some daily weather stats for quality filters
dailyPrecip <- data.frame( doy = weatherDaily$doy,
                           dayPrec = weatherDaily$Prec)

sap_all <- left_join(sap_all, dailyPrecip, by="doy")

sap_analysis <- sap_all %>%
  filter(dayPrec <= 4) %>% # only take days with trace precip amounts less than or equal to 4 mm
  filter(hour >= 6 & hour <= 19 ) %>%
  filter(VPD_hr > 0.25)

ggplot(sap_analysis, aes(VPD_hr, T.L.hr, color=species))+
  geom_point()

# check for hysteresis in hourly
ggplot(sap_analysis %>% filter(doy == 168), aes(VPD_hr, T.L.hr, color=species))+
  geom_point()+
  geom_path()

ggplot(sap_analysis %>% filter(doy == 168), aes(VPD_m4, T.L.hr, color=species))+
  geom_point()+
  geom_path()
ggplot(sap_analysis %>% filter(doy == 168), aes(VPD_hr, T.L.hr, color=species))+
  geom_point()

ggplot(sap_analysis %>% filter(doy == 168), aes(VPD_m4, T.L.hr, color=species))+
  geom_point()

ggplot(sap_analysis %>% filter(doy == 173), aes(VPD_m1, T.L.hr, color=species))+
  geom_point()+
  geom_path()

ggplot(sap_analysis %>% filter(doy == 173), aes(S_Rad, T.L.hr, color=species))+
  geom_point()+
  geom_path()

ggplot(sap_analysis %>% filter(doy == 173), aes(SRad_m4, T.L.hr, color=species))+
  geom_point()+
  geom_path()

ggplot(sap_analysis %>% filter(doy == 173), aes(S_Rad, T.L.hr, color=species))+
  geom_point()

ggplot(sap_analysis %>% filter(doy == 173), aes(VPD_hr, T.L.hr, color=species))+
  geom_point()
# evaluate degree of hysteresis
bass_analysis <- sap_analysis %>%
  filter(species == "basswood")

hem_analysis <- sap_analysis %>%
  filter(species == "hemlock")

##### Assess hysteresis ----
# current

plot(log(bass_analysis$VPD_hr),bass_analysis$T.L.hr)
plot(log(bass_analysis$VPD_m1),bass_analysis$T.L.hr)
plot(log(bass_analysis$VPD_m2),bass_analysis$T.L.hr)
plot(log(bass_analysis$VPD_m3),bass_analysis$T.L.hr)
plot(log(bass_analysis$VPD_m4),bass_analysis$T.L.hr)

plot(log(hem_analysis$VPD_hr),hem_analysis$T.L.hr)
plot(log(hem_analysis$VPD_m1),hem_analysis$T.L.hr)
plot(log(hem_analysis$VPD_m2),hem_analysis$T.L.hr)
plot(log(hem_analysis$VPD_m3),hem_analysis$T.L.hr)

modDay <- bass_analysis %>%
  filter(doy == 173)

mod.CB <- lm(modDay$T.L.hr ~ log(modDay$VPD_hr))
summary(mod.CB)
mod.m1B <- lm(modDay$T.L.hr ~ log(modDay$VPD_m1))
summary(mod.m1B)

mod.m2B <- lm(modDay$T.L.hr ~ log(modDay$VPD_m2))
summary(mod.m2B)

mod.m3B <- lm(modDay$T.L.hr ~ log(modDay$VPD_m4))
summary(mod.m4B)

# daily patterns in data



ggplot(Tc.L.day, aes(maxVPD, L.day, color=species))+
  geom_point()

ggplot(Tc.L.day, aes(maxVPD, L.day.m2, color=species))+
  geom_point()

ggplot(Tc.L.day, aes(SWC, L.day, color=species))+
  geom_point()

ggplot(Tc.L.day, aes(AirT, L.day, color=species))+
  geom_point()


ggplot(Tc.L.day, aes(max_SW, L.day, color=species))+
  geom_point()
