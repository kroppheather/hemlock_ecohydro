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


source(paste0(dirScript, "/sapflux.r"))

#### soil and weather data ----
# soilDaily and soilHourly give soil moisture and temperature
# weatherDaily and weatherHourly have hourly and daily stats

source(paste0(dirScript, "/soil_weather.r"))


#### explore basic data patterns  ----
TreeInfo <- unique(data.frame(species=sensors$Tree.Type,
                  DBH = sensors$DBH..cm., 
                  LA.m2 = sensors$LA.m2))

# daily data
T.L.dayW <- left_join(weatherDaily, T.L.day, by=c("doy"))
Tc.L.day <- left_join(T.L.dayW , soilDaily, by="doy")

# join in some daily weather stats for quality filters
dailyPrecip <- data.frame( doy = weatherDaily$doy,
                           dayPrec = weatherDaily$Prec)
weeklyPrecip <- rep(NA,6)
for(i in 7:nrow(dailyPrecip)){
  weeklyPrecip[i] <- sum(dailyPrecip$dayPrec[(i-6):i])
}

dailyPrecip$weekPr <- weeklyPrecip

corTest <- na.omit(left_join(soilDaily,dailyPrecip, by="doy"))

plot(corTest$SWC, corTest$weekPr)
cor(corTest$SWC, corTest$weekPr)


T_L_day <- left_join(Tc.L.day, dailyPrecip, by="doy")
# note that L per day works out to mm/day 
ggplot(T_L_day, aes(doy, El_day, color=species))+
         geom_point()+
  geom_line()


ggplot(T_L_day, aes(aveVPD, El_day, color=species))+
  geom_point()

ggplot(T_L_day, aes(aveVPD, El_day, color=species))+
  geom_point()

ggplot(T_L_day, aes(maxVPD, El_day, color=species))+
  geom_point()

ggplot(T_L_day, aes(weekPr, El_day, color=species))+
  geom_point()

ggplot(T_L_day%>%filter(species == "basswood"), aes(log(weekPr), El_day))+
  geom_point()

ggplot(T_L_day%>%filter(species == "hemlock"), aes(log(weekPr), El_day))+
  geom_point()

ggplot(T_L_day, aes(SWC, weekPr))+
  geom_point()

ggplot(T_L_day, aes(log(SWC), El_day, color=species))+
  geom_point()

ggplot(T_L_day%>%filter(species == "hemlock"), aes(log(SWC), El_day))+
  geom_point()


ggplot(T_L_day%>%filter(species == "basswood"), aes(log(SWC), El_day))+
  geom_point()

ggplot(T_L_day, aes(s_temp, El_day, color=species))+
  geom_point()

ggplot(T_L_day, aes(doy, El_day, color=species))+
  geom_point()
